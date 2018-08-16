module Data.Regex.Mono.Pretty
    ( prettyRE
    , prettyByteSet
    ) where

import Data.ByteSet
import Data.Regex.Mono.Core

import Data.BooleanAlgebra

import Data.Monoid (Endo(..))
import Data.Word (Word8)
import GHC.Exts (toList)

prettyRE :: RE -> String
prettyRE r = showREPrec 0 r ""

prettyByteSet :: ByteSet -> String
prettyByteSet cc = showByteSet cc ""

showREPrec :: Int -> RE -> ShowS
showREPrec prec r0 = case r0 of
    Null -> showString "[]"
    Epsilon -> showString ""
    Symbol cc -> showByteSet cc
    Complement r -> showParen (prec > 9) $ showString "!" . showREPrec 10 r
    Star r -> showParen (prec > 10) $ showREPrec 11 r . showString "*"
    Or r s -> showParen (prec > 1) $ showREPrec 1 r . showString "|" . showREPrec 1 s
    And r s -> showParen (prec > 2) $ showREPrec 2 r . showString "&" . showREPrec 2 s
    Then r s -> showParen (prec > 3) $ showREPrec 3 r . showREPrec 3 s

showByteSet :: ByteSet -> ShowS
showByteSet cc = case order cc of
    0 -> showString "[]"
    256 -> showString "."
    1 -> let Just b = choose cc in showLiteral b
    n -> if n > 128
            then showString "[^" . showAsClass (complement cc) . showString "]"
            else showString "[" . showAsClass cc . showString "]"

showAsClass :: ByteSet -> ShowS
showAsClass = appEndo . foldMap Endo . map showLiteral . toList

showLiteral :: Word8 -> ShowS
showLiteral = showString . init . tail . show . w2c

w2c :: Word8 -> Char
w2c = toEnum . fromEnum
