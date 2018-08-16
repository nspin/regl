{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Semiring.Extra
    ( Plus(..)
    , Times(..)
    ) where

import Data.Semiring

newtype Plus a = Plus { getPlus :: a }
    deriving (Semiring)

instance Semiring a => Semigroup (Plus a) where
    (<>) = (<+>)

instance Semiring a => Monoid (Plus a) where
    mempty = Plus zero

newtype Times a = Times { getTimes :: a }
    deriving (Semiring)

instance Semiring a => Semigroup (Times a) where
    (<>) = (<.>)

instance Semiring a => Monoid (Times a) where
    mempty = Times one
