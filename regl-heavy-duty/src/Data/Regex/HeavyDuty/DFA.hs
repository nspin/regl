module Data.Regex.HeavyDuty.DFA
    ( DFA(..)
    , DFAState
    , compile
    , dfaView
    , DFAView(..)
    ) where

import RegExp.RegExp
import RegExp.Language
import RegExp.Derivative

import Data.GSet
import Data.Semiring
import Data.KleeneAlgebra
import Data.BooleanAlgebra

import Data.Word (Word8)
import GHC.Exts (toList)
import qualified Data.Array.Unboxed as U
import qualified Data.Set

type DFAState = Int

data DFA = DFA
    { dfaSize :: Int
    , dfaStart :: DFAState
    , dfaTransition :: U.Array (DFAState, Word8) DFAState
    , dfaAccept :: U.Array DFAState Bool
    } deriving (Show)

compile :: Language Word8 -> DFA
compile lang = DFA
    { dfaSize = size
    , dfaStart = state (regexp lang)
    , dfaTransition = U.array ((0, 0), (size - 1, 255))
        [ ((i, c), i') | (i, d) <- zip [0..] (Data.Set.toAscList derivatives), (c, i') <- row d ]
    , dfaAccept = U.listArray (0, size - 1)
        [ nullable d | d <- Data.Set.toAscList derivatives ]
    }
  where
    size :: Int
    size = Data.Set.size derivatives

    state :: RegExp Word8 -> DFAState
    state r = Data.Set.findIndex r derivatives

    derivatives :: Data.Set.Set (RegExp Word8)
    derivatives = regexp zero `Data.Set.insert` allDerivatives (regexp lang)

    row :: RegExp Word8 -> [(Word8, DFAState)]
    row r = [ (c, 0) | c <- toList $ complement $ ors $ next r ] <> do
        p <- Data.Set.toList (next r)
        Just c0 <- [choose p]
        let i = state (derivative c0 r)
        c <- toList p
        return (c, i)

data DFAView = DFAView
    { dfaViewStart :: DFAState
    , dfaViewTransition :: DFAState -> Word8 -> DFAState
    , dfaViewAccept :: DFAState -> Bool
    , dfaViewReject :: DFAState -> Bool
    }

dfaView :: DFA -> DFAView
dfaView dfa = DFAView
    { dfaViewStart = dfaStart dfa
    , dfaViewTransition = curry (dfaTransition dfa U.!)
    , dfaViewAccept = (dfaAccept dfa U.!)
    , dfaViewReject = (== 0)
    }
