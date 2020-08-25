module Data.Regex.Streaming.HFA
    ( HFASuperposition(..)
    , hfaCreate
    , hfaStep
    ) where

import Data.Regex.DFA
import Data.Regex.DFA.Analysis
import Data.Regex.Internal.DistanceBound

import Control.Monad.Primitive
import Data.Primitive.MutVar
import Data.Primitive.Array
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import qualified Data.Array.Unboxed as U
import Data.Word
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import Data.Function (on)

-- data Superposition = Superposition
--     { superpositionArray :: U.UArray Int Bool
--     , superpositionList :: [Int] -- invariant: sorted
--     }

-- superpose :: [Int] {- sorted -} -> Superposition
-- superpose = 

-- (MutablePrimArray (PrimState m) a)

-- type Superposition = VU.Vector DFAState -- sorted
data HFASuperposition = HFASuperposition
    { hfaSuperpositionStates :: [DFAState] -- sorted
    , hfaSuperpositionAccept :: Bool
    , hfaSuperpositionDistanceBound :: DistanceBound
    }

instance Eq HFASuperposition where
    (==) = (==) `on` hfaSuperpositionStates

instance Ord HFASuperposition where
    compare = compare `on` hfaSuperpositionStates

createSuperposition :: DFAWithDistanceBounds -> [DFAState] {- sorted -} -> HFASuperposition
createSuperposition dfaWithDistanceBounds@(DFAWithDistanceBounds dfa bounds) states = HFASuperposition
    states
    (any (dfaAccept dfa) states)
    (maximum (map (dfaDistanceBound dfaWithDistanceBounds) states))

-- TODO faster
nextSuperposition :: DFAWithDistanceBounds -> HFASuperposition -> Word8 -> HFASuperposition
nextSuperposition dfaWithDistanceBounds@(DFAWithDistanceBounds dfa bounds) sup w =
    createSuperposition dfaWithDistanceBounds $ IS.toAscList $ IS.fromList [ dfaStep dfa s w | s <- (hfaSuperpositionStates sup) ]

data HFACachedSuperposition s = HFACachedSuperposition
    { cachedSuperposition :: HFASuperposition
    , cachedEdges :: VM.MVector s (Maybe (HFACachedSuperposition s))
    }

newCachedSuperposition :: PrimMonad m => HFASuperposition -> m (HFACachedSuperposition (PrimState m))
newCachedSuperposition sup = do
    edges <- VM.replicate 256 Nothing
    return $ HFACachedSuperposition sup edges

data HFACachedSuperpositions s = HFACachedSuperpositions
    { cachedSuperpositions :: MutVar s (M.Map HFASuperposition (HFACachedSuperposition s))
    }

data HFA s = HFA
    { hfaDFA :: DFAWithDistanceBounds
    , hfaCache :: HFACachedSuperpositions s
    , hfaSuperposition :: MutVar s (HFACachedSuperposition s)
    }

hfaCreate :: PrimMonad m => DFAWithDistanceBounds -> m (HFA (PrimState m))
hfaCreate dfaWithDistanceBounds = do
    let sup = createSuperposition dfaWithDistanceBounds [0]
    csup <- newCachedSuperposition sup
    cache <- newMutVar (M.singleton sup csup)
    var <- newMutVar csup
    return $ HFA dfaWithDistanceBounds (HFACachedSuperpositions cache) var

hfaStep :: PrimMonad m => HFA (PrimState m) -> Word8 -> m HFASuperposition
hfaStep hfa w = do
    csup <- readMutVar (hfaSuperposition hfa)
    m <- VM.read (cachedEdges csup) (fromIntegral w)
    csup' <- case m of
        Just csup' -> do
            return csup'
        Nothing -> do
            let sup' = nextSuperposition (hfaDFA hfa) (cachedSuperposition csup) w
            csups <- readMutVar (cachedSuperpositions (hfaCache hfa))
            csup' <- case M.lookup sup' csups of
                Just csup' -> do
                    return csup'
                Nothing -> do
                    csup' <- newCachedSuperposition sup'
                    writeMutVar (cachedSuperpositions (hfaCache hfa)) (M.insert sup' csup' csups)
                    return csup'
            VM.write (cachedEdges csup) (fromIntegral w) (Just csup')
            return csup'
    writeMutVar (hfaSuperposition hfa) csup'
    return (cachedSuperposition csup')
