{-# LANGUAGE FlexibleInstances #-}

module Data.Regex.Mono.Combinators.Char
    ( text
    , bytes
    , byteRange
    ) where

import Data.Regex.Mono.Core
import Data.ByteSet

import Data.Semiring
import Data.Semiring.Extra
import Data.KleeneAlgebra
import Data.BooleanAlgebra

import Data.Word (Word8)
import Data.Foldable (foldl')
import Data.String
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

text :: T.Text -> RE
text = bytes . TE.encodeUtf8

bytes :: B.ByteString -> RE
bytes = foldl' (<.>) one . map (literal . singleton) . B.unpack

byteRange :: Word8 -> Int -> ByteSet
byteRange off len = foldl' (<+>) zero . take len $ map singleton [off..]

instance IsString RE where
    fromString s = text (T.pack s)
