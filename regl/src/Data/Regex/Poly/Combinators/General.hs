module Data.Regex.Poly.Combinators.General
    ( choice
    , between
    , sepBy
    , sepBy1
    , many1

    , count
    , countMin
    , countMax
    , countRange

    , many_
    , some_
    , optional_
    , count_
    , countMin_
    , countMax_
    , countRange_
    ) where

import Control.Applicative
import Control.Monad (void)
import Data.List (genericReplicate)
import Data.List.NonEmpty (NonEmpty(..), fromList)


-- General

choice :: Alternative f => [f a] -> f a
choice = foldr (<|>) empty

between :: Applicative f => f bra -> f ket -> f a -> f a
between bra ket v = bra *> v <* ket

sepBy :: Alternative f => f a -> f b -> f [a]
sepBy v s = (:) <$> v <*> many (s *> v) <|> pure []

sepBy1 :: Alternative f => f a -> f b -> f (NonEmpty a)
sepBy1 v s = (:|) <$> v <*> many (s *> v)

many1 :: Alternative f => f a -> f (NonEmpty a)
many1 = fmap fromList . some


count :: (Applicative f, Integral n) => n -> f a -> f [a]
count n v = sequenceA (genericReplicate n v)

countMin :: (Alternative f, Integral n) => n -> f a -> f [a]
countMin n v = (<>) <$> count n v <*> many v

countMax :: (Alternative f, Integral n) => n -> f a -> f [a]
countMax n v = go n
  where
    go 0 = pure []
    go i = (:) <$> v <*> go (i - 1) <|> pure []

countRange :: (Alternative f, Integral n) => n -> n -> f a -> f [a]
countRange n1 n2 v = (<>) <$> count n1 v <*> countMax (n2 - n1) v


many_ :: Alternative f => f a -> f ()
many_ = void . many

some_ :: Alternative f => f a -> f ()
some_ = void . some

optional_ :: Alternative f => f a -> f ()
optional_ = void . optional

count_ :: (Applicative f, Integral n) => n -> f a -> f ()
count_ 0 _ = pure ()
count_ n v = v *> count_ (n - 1) v

countMin_ :: (Alternative f, Integral n) => n -> f a -> f ()
countMin_ n v = count_ n v *> many_ v

countMax_ :: (Alternative f, Integral n) => n -> f a -> f ()
countMax_ n v = go n
  where
    go 0 = pure ()
    go i = v *> go (i - 1) <|> pure ()

countRange_ :: (Alternative f, Integral n) => n -> n -> f a -> f ()
countRange_ n1 n2 v = count_ n1 v *> countMax_ (n2 - n1) v
