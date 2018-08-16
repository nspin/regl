{-# LANGUAGE FlexibleInstances #-}

module Data.Regex.Poly.Combinators.Char
    ( string

    , digit
    , letter
    , space

    , decimal
    , hexadecimal
    , signed
    ) where

import Data.Regex.Poly.Core
import Data.Regex.Poly.Combinators
-- import Data.Regex.Poly.Combinators.General

import Control.Applicative
import Data.Bits
import Data.Char
import Data.Foldable
import Data.String

string :: String -> RE Char String
string = traverse symbol

instance IsString (RE Char String) where
    fromString = string

digit :: RE Char Char
digit = satisfy isDigit

letter :: RE Char Char
letter = satisfy isAlpha

space :: RE Char Char
space = satisfy isSpace

decimal :: Integral a => RE Char a
decimal = foldl' step 0 <$> many digit
  where
    step a c = a * 10 + fromIntegral (ord c - 48)

hexadecimal :: (Integral a, Bits a) => RE Char a
hexadecimal = foldl' step 0 <$> many (satisfy isHexDigit)
  where
    step a c
        | w >= 48 && w <= 57  = (a `shiftL` 4) .|. fromIntegral (w - 48)
        | w >= 97             = (a `shiftL` 4) .|. fromIntegral (w - 87)
        | otherwise           = (a `shiftL` 4) .|. fromIntegral (w - 55)
      where
        w = ord c

signed :: Num a => RE Char a -> RE Char a
signed r = (negate <$> (symbol '-' *> r)) <|> (symbol '+' *> r) <|> r
