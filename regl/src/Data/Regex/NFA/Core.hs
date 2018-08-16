{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Regex.NFA.Core
    ( NFA(..)
    , NFAState
    , NFASuperposition
    , nfaSize
    , nfaAccept
    , nfaStart
    , nfaStep
    ) where

import Data.ByteSet
import Data.Regex.Mono
import Data.Regex.Mono.Pretty

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import qualified Data.Array.Unboxed as AU

type NFAState = Int
type NFASuperposition = [NFAState]

data NFA = NFA
    { nfaAcceptArray :: AU.Array NFAState Bool
    , nfaStepArray :: AU.Array (NFAState, Word8) NFASuperposition
    } deriving (Show, Generic, NFData)

nfaSize :: NFA -> Int
nfaSize nfa = let (0, end) = AU.bounds (nfaAcceptArray nfa) in end + 1

nfaAccept :: NFA -> NFAState -> Bool
nfaAccept nfa = (nfaAcceptArray nfa AU.!)

nfaStart :: NFASuperposition
nfaStart = [0]

nfaStep :: NFA -> NFAState -> Word8 -> NFASuperposition
nfaStep nfa = curry (nfaStepArray nfa AU.!)

-- import Control.Monad (unless)
-- import Control.Monad.ST
-- import Control.Monad.Trans.State.Strict (State, runState, execState, get, put)
-- import Data.Foldable (for_, traverse_)
-- import Data.Monoid (Endo(..))
-- import Data.Traversable (for)
-- import Data.Word (Word8)
-- import GHC.Exts (toList)
-- import qualified Data.Array.Unboxed.Mutable as AUM
-- import qualified Data.IntSet as IS
-- import qualified Data.Map as M
-- import qualified Data.Vector as V
-- import qualified Data.Vector.Unboxed.Mutable as VM

-- nfaStepsWith :: VM.MVector (PrimState m) Bool -> NFA -> NFASuperposition -> Word8 -> NFASuperposition
-- nfaStepsWith set nfa sup b = go [] [ nfaStepArray nfa U.! (state, b) | state <- sup ]
--   where
--     go acc [] = return acc
--     go acc (state:states) = do
--         seen <- VM.read set state
--         if seen
--             then do
--                 go acc states
--             else do
--                 VM.write set state True
--                 go (state:acc) states

-- nfaSteps :: NFA -> NFASuperposition -> Word8 -> NFASuperposition
-- nfaSteps nfa sup b = runST $ do
--     set <- VM.replicate (nfaSize nfa) False
--     nfaStepWith set nfa sup b

-- nfaSteps :: NFA -> [NFASuperposition] -> Word8 -> [NFASuperposition]
-- nfaSteps nfa sups b = runST $ do
--     set <- VM.replicate (nfaSize nfa) False
--     forM nfaStepWith set nfa sup b
