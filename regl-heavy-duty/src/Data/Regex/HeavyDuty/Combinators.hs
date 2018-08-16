{-# LANGUAGE FlexibleInstances #-}

module Data.Regex.HeavyDuty.Combinators
    ( literal
    , intersection
    , complement_
    , difference
    , ors

    , bytes
    , text
    , byteRange

    , plus
    , optional
    , sum_
    , between
    , sepBy
    , sepBy1

    , product_
    , count
    , countMin
    , countMax
    , countRange

    , module Data.GSet
    , module Data.Semiring
    , module Data.KleeneAlgebra
    , module Data.BooleanAlgebra
    ) where

import RegExp.RegExp (CharacterClass, rLiteral)
import RegExp.Language (Language, language, regexp)
import qualified RegExp.Operations as O

import Data.GSet
import Data.Semiring
import Data.KleeneAlgebra
import Data.BooleanAlgebra

import Data.Foldable (foldl')
import Data.String
import Data.List (genericReplicate)
import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

literal :: CharacterClass Word8 -> Language Word8
literal = language . rLiteral

intersection :: (Ord c, Enum c, Bounded c) => Language c -> Language c -> Language c
intersection a b = language $ regexp a `O.intersection` regexp b

difference :: (Ord c, Enum c, Bounded c) => Language c -> Language c -> Language c
difference a b = language $ regexp a `O.difference` regexp b

complement_ :: (Ord c, Enum c, Bounded c) => Language c -> Language c
complement_ = language . O.complement . regexp


bytes :: B.ByteString -> Language Word8
bytes = foldl' (<.>) one . map (literal . singleton) . B.unpack

text :: T.Text -> Language Word8
text = bytes . TE.encodeUtf8

instance IsString (Language Word8) where
    fromString s = text (T.pack s)

byteRange :: Word8 -> Int -> CharacterClass Word8
byteRange off len = foldl' (<+>) zero . take len $ map singleton [off..]


-- General

newtype Plus a = Plus { getPlus :: a }
newtype Times a = Times { getTimes :: a }

instance Semiring a => Semigroup (Plus a) where
    Plus x <> Plus y = Plus (x <+> y)
instance Semiring a => Monoid (Plus a) where
    mempty = Plus zero

instance Semiring a => Semigroup (Times a) where
    Times x <> Times y = Times (x <.> y)
instance Semiring a => Monoid (Times a) where
    mempty = Times one
    

plus :: KleeneAlgebra a => a -> a
plus a = a <.> star a

optional :: Semiring a => a -> a
optional a = a <+> one

sum_ :: (Semiring a, Foldable t) => t a -> a
sum_ = getPlus . foldMap Plus

between :: Semiring a => a -> a -> a -> a
between bra ket a = bra <.> a <.> ket

sepBy :: KleeneAlgebra a => a -> a -> a
sepBy s a = optional $ sepBy1 s a

sepBy1 :: KleeneAlgebra a => a -> a -> a
sepBy1 s a =  a <.> star (s <.> a)


product_ :: (Semiring a, Foldable t) => t a -> a
product_ = getTimes . foldMap Times

count :: (Semiring a, Integral n) => n -> a -> a
count n a = product_ (genericReplicate n a)

countMin :: (KleeneAlgebra a, Integral n) => n -> a -> a
countMin n a = count n a <.> star a

countMax :: (Semiring a, Integral n) => n -> a -> a
countMax n a = go n
  where
    go 0 = one
    go i = a <.> go (i - 1) <+> one

countRange :: (KleeneAlgebra a, Integral n) => n -> n -> a -> a
countRange n1 n2 a = count n1 a <.> countMax (n2 - n1) a
