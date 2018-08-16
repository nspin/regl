{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}

module Data.Regex.DFA.Core
    ( DFA(..)
    , DFAState
    , dfaStart
    , dfaSize
    , dfaAccept
    , dfaStep

    , TrivialityView(..)
    , trivialityView

    , compile

    , debugPrintDFA
    ) where

import Data.ByteSet
import Data.Regex.Mono
import Data.Regex.Mono.Pretty

import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Control.Monad.Trans.State.Strict (State, runState, execState, get, put)
import Data.Foldable (for_, traverse_)
import Data.Monoid (Endo(..))
import Data.Traversable (for)
import Data.Word (Word8)
import GHC.Exts (toList)
import GHC.Generics (Generic)
import qualified Data.Array.Unboxed as U
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

type DFAState = Int

data DFA = DFA
    { dfaAcceptLoop :: Maybe DFAState
    , dfaRejectLoop :: Maybe DFAState
    , dfaAcceptArray :: U.Array DFAState Bool
    , dfaStepArray :: U.Array (DFAState, Word8) DFAState
    } deriving (Show, Generic, NFData)

dfaStart :: DFAState
dfaStart = 0

dfaSize :: DFA -> Int
dfaSize dfa = let (0, end) = U.bounds (dfaAcceptArray dfa) in end + 1

dfaAccept :: DFA -> DFAState -> Bool
dfaAccept dfa = (dfaAcceptArray dfa U.!)

dfaStep :: DFA -> DFAState -> Word8 -> DFAState
dfaStep dfa = curry (dfaStepArray dfa U.!)

data TrivialityView = Trivial Bool
                    | NonTrivial DFAState -- ^ reject loop

trivialityView :: DFA -> TrivialityView
trivialityView dfa = case dfaRejectLoop dfa of
    Nothing -> Trivial True
    Just i -> case dfaSize dfa of
        1 -> Trivial False
        _ -> NonTrivial i

compile :: RE -> DFA
compile r0 = DFA
    { dfaAcceptLoop = acceptLoop
    , dfaRejectLoop = rejectLoop
    , dfaAcceptArray = U.listArray (0, size - 1)
        [ ok | (_, ok, _) <- V.toList states ]
    , dfaStepArray = U.array ((0, 0), (size - 1, 255))
        [ ((i, c), i')
        | (i, (_, _, parts)) <- zip [0..] (V.toList states)
        , (part, i') <- parts
        , c <- toList part
        ]
    }
  where
    (acceptLoop, rejectLoop, states) = buildDFA r0
    size = V.length states

buildDFA :: RE -> (Maybe DFAState, Maybe DFAState, V.Vector (RE, Bool, [(ByteSet, DFAState)]))
buildDFA r = (acceptLoop, rejectLoop, dfa2)
  where
    dfa0 = buildDumbDFA r
    (acceptLoop, dfa1) = removeExcessLoops True dfa0
    (rejectLoop, dfa2) = removeExcessLoops False dfa1

-- remove excess accept or reject loops
removeExcessLoops :: Bool -> V.Vector (RE, Bool, [(ByteSet, DFAState)]) -> (Maybe DFAState, V.Vector (RE, Bool, [(ByteSet, DFAState)]))
removeExcessLoops isAccept oldDFA = case IS.minView loops of
    Nothing -> (Nothing, oldDFA)
    Just (i0, rest) ->
        let newSize = oldSize - IS.size rest
            newToOld = V.fromListN newSize [ i | i <- [0 .. oldSize - 1], not (IS.member i rest) ]
            oldToNew = fromAssocs oldSize $ zip (V.toList newToOld) [0..] ++ [ (i, i0) | i <- IS.toList loops ]
            translateEdges edges = [ (cc, oldToNew V.! i) | (cc, i) <- edges ]
            newDFA = V.fromListN newSize
                [ let (r, ok, edges) = oldDFA V.! oldI
                  in (r, ok, translateEdges edges)
                | oldI <- V.toList newToOld
                ]
        in (Just i0, newDFA)
  where
    oldSize = V.length oldDFA
    loops = invertWithin 0 (oldSize - 1) nonLoops
    nonLoops = reachableFrom (reverseEdges V.!) $ V.toList $ V.findIndices (\(_, x, _) -> x /= isAccept) oldDFA
    reverseEdges = fromAssocsAccum [] (:) oldSize
        [ (to, from)
        | (from, (_, _, edges)) <- zip [0..] (V.toList oldDFA)
        , (_, to) <- edges
        ]

invertWithin :: Int -> Int -> IS.IntSet -> IS.IntSet
invertWithin lo hi s = IS.fromDistinctAscList [ i | i <- [lo .. hi], not (IS.member i s) ]

reachableFrom :: (Int -> [Int]) -> [Int] -> IS.IntSet
reachableFrom edges starts = execState (traverse_ go starts) IS.empty
  where
    go i = do
        visited <- get
        unless (IS.member i visited) $ do
            put (IS.insert i visited)
            traverse_ go (edges i)

fromAssocs :: Int -> [(Int, a)] -> V.Vector a
fromAssocs n assocs = V.create $ do
    v <- MV.new n
    for_ assocs $ \(i, a) ->
        MV.write v i a
    return v

fromAssocsAccum :: a -> (b -> a -> a) -> Int -> [(Int, b)] -> V.Vector a
fromAssocsAccum initial combine n assocs = V.create $ do
    v <- MV.replicate n initial
    for_ assocs $ \(i, a) ->
        MV.modify v (combine a) i
    return v

-- start state is 0
-- dumb because may have multiple accept and reject loops (multiple states for 'zero' and 'star one')
buildDumbDFA :: RE -> V.Vector (RE, Bool, [(ByteSet, DFAState)])
buildDumbDFA r0 = fromAssocs (M.size finalRes) finalAssocs
  where
    (0, (finalRes, finalAssocs)) = runState (addRE r0) (M.empty, [])

    addRE :: RE -> State (M.Map RE DFAState, [(DFAState, (RE, Bool, [(ByteSet, DFAState)]))]) DFAState
    addRE r = do
        (res, assocs) <- get
        case M.lookup r res of
            Just i -> return i
            Nothing -> do
                let i = M.size res
                put (M.insert r i res, assocs)
                edges <- for (transitions r) $ \(cc, r') -> do
                    i' <- addRE r'
                    return (cc, i')
                (res', assocs') <- get
                put $ (res', (i, (r, nullable r, edges)):assocs')
                return i

debugPrintDFA :: RE -> IO ()
debugPrintDFA r0 = putStrLn . ($ "") $
    (appEndo . foldMap Endo)
        [ showChar ' '
        . showString (if i == 0 then "^ " else "  ")
        . showString (if | acceptLoop == Just i -> "1!"
                         | rejectLoop == Just i -> "0!"
                         | isAccept -> "$ "
                         | otherwise -> "  ")
        . showChar ' '
        . shows i . showString ": " . quoted r
        . (appEndo . foldMap Endo)
            [ showChar '\t' . showString (prettyByteSet cc) . showString " -> " . shows i'
            | (cc, i') <- st
            ]
        . nl
        | (i, (r, isAccept, st)) <- zip [0..] (V.toList states)
        ]
  where
    (acceptLoop, rejectLoop, states) = buildDFA r0
    quoted r = showChar '\'' . showString (prettyRE r) . showChar '\''
    nl = showChar '\n'
