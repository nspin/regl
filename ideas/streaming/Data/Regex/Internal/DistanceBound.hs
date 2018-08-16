{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Regex.Internal.DistanceBound
    ( DistanceBound
    , unbounded
    ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Semigroup
import Data.Monoid
import Data.Function

newtype DistanceBound = DistanceBound { getDistanceBound :: Maybe Integer }
    deriving (Eq, Generic, NFData)

instance Show DistanceBound where
    show (DistanceBound Nothing) = "unbounded"
    show (DistanceBound (Just x)) = show x

instance Semigroup DistanceBound where
    DistanceBound a <> DistanceBound b = DistanceBound $ max <$> a <*> b

instance Monoid DistanceBound where
    mempty = 0

instance Ord DistanceBound where
    compare (DistanceBound Nothing) (DistanceBound Nothing) = EQ
    compare (DistanceBound Nothing) _ = GT
    compare _ (DistanceBound Nothing) = LT
    compare (DistanceBound (Just x)) (DistanceBound (Just y)) = compare x y

instance Num DistanceBound where
    DistanceBound a + DistanceBound b = DistanceBound $ (+) <$> a <*> b
    fromInteger = DistanceBound . Just

unbounded :: DistanceBound
unbounded = DistanceBound Nothing
