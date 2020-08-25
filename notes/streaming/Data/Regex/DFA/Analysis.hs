{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Regex.DFA.Analysis
    ( DFAWithDistanceBounds(..)
    , withDistanceBounds
    , dfaDistanceBound
    ) where

import Data.Regex.DFA
import Data.Regex.Internal.DistanceBound

import Control.DeepSeq (NFData)
import qualified Data.Array as A
import GHC.Generics (Generic)

data DFAWithDistanceBounds = DFAWithDistanceBounds
    { withoutDistanceBounds :: DFA
    , getDistanceBounds :: A.Array DFAState DistanceBound
    } deriving (Show, Generic, NFData)

withDistanceBounds :: DFA -> DFAWithDistanceBounds
withDistanceBounds dfa = DFAWithDistanceBounds dfa $ A.listArray (0, n - 1) (replicate n unbounded)
  where
    n = dfaSize dfa

dfaDistanceBound :: DFAWithDistanceBounds -> DFAState -> DistanceBound
dfaDistanceBound dfa s = getDistanceBounds dfa A.! s
