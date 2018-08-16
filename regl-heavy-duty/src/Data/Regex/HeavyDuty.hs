module Data.Regex.HeavyDuty
    ( Language
    , CharacterClass
    , Word8

    , matchLBS
    , showLang

    , module Data.Regex.HeavyDuty.Combinators
    , module Data.Regex.HeavyDuty.DFA
    ) where

import Data.Regex.HeavyDuty.Combinators
import Data.Regex.HeavyDuty.DFA

import RegExp.RegExp
import RegExp.Language
import RegExp.Derivative

import Data.GSet
import Data.Semiring
import Data.KleeneAlgebra
import Data.BooleanAlgebra

import Data.Word (Word8)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC

showLang :: Language Word8 -> String
showLang = show . regexp

matchLBS :: DFA -> L.ByteString -> Bool
matchLBS dfa lbs = go (L.toChunks lbs) (dfaViewStart dfav)
  where
    dfav = dfaView dfa
    go [] s = dfaViewAccept dfav s
    go (c:cs) s = go' (B.length c) 0 c cs s
    go' _ _ _ _  0 = False
    go' 0 _ _ cs s = go cs s
    go' n i c cs s = go' (n - 1) (i + 1) c cs (dfaViewTransition dfav s (B.index c i))

test :: String -> Language Word8 -> Bool
test s r = matchLBS (compile r) (LC.pack s)
