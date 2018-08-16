module Data.Regex.Mono.Combinators
    ( difference
    , anything

    , Language(..)

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
    ) where

import Data.Regex.Mono.Core

import Data.Semiring
import Data.Semiring.Extra
import Data.KleeneAlgebra
import Data.BooleanAlgebra

import Data.List (genericReplicate)

difference :: RE -> RE -> RE
difference r s = and_ r (not_ s)

anything :: RE
anything = not_ zero

newtype Language = Language { getLanguage :: RE }

instance Semiring Language where
    zero = Language Null
    one = Language anything
    Language r <+> Language s = Language (r <+> s)
    Language r <.> Language s = Language (and_ r s)

-- instance DetectableZero Language where
--     isZero = isZero . getLanguage

instance BooleanAlgebra Language where
    complement = Language . not_ . getLanguage

-- General

plus :: KleeneAlgebra a => a -> a
plus a = a <.> star a

optional :: Semiring a => a -> a
optional a = a <+> one

sum_ :: (Semiring a, Foldable t) => t a -> a
sum_ = getPlus . foldMap Plus

product_ :: (Semiring a, Foldable t) => t a -> a
product_ = getTimes . foldMap Times

between :: Semiring a => a -> a -> a -> a
between bra ket a = bra <.> a <.> ket

sepBy :: KleeneAlgebra a => a -> a -> a
sepBy s a = optional $ sepBy1 s a

sepBy1 :: KleeneAlgebra a => a -> a -> a
sepBy1 s a =  a <.> star (s <.> a)

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
