{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Data.ByteSet
    ( ByteSet
    , order
    , member
    , choose
    , singleton
    , fromPredicate
    ) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

import Data.Semiring
import Data.BooleanAlgebra

import Control.DeepSeq
import Data.Foldable (for_)
import Data.Word (Word8)
import GHC.Exts (IsList(..))
import GHC.Generics (Generic)

newtype ByteSet = ByteSet { getByteSet :: V.Vector Bool }
    deriving (Eq, Ord, Generic, NFData)

instance Semiring ByteSet where
    zero = ByteSet $ V.replicate 256 zero
    one = ByteSet $ V.replicate 256 one
    ByteSet u <+> ByteSet v = ByteSet $ V.zipWith (<+>) u v
    ByteSet u <.> ByteSet v = ByteSet $ V.zipWith (<.>) u v

instance BooleanAlgebra ByteSet where
    complement = ByteSet . V.map complement . getByteSet

instance DetectableZero ByteSet where
    isZero = not . V.or . getByteSet

order :: ByteSet -> Int
order = V.length . V.filter id . getByteSet

member :: Word8 -> ByteSet -> Bool
member b (ByteSet v) = v V.! fromEnum b

choose :: ByteSet -> Maybe Word8
choose = fmap toEnum . V.findIndex id . getByteSet

singleton :: Word8 -> ByteSet
singleton b = ByteSet $ V.create $ do
    v <- VM.new 256
    VM.write v (fromEnum b) True
    return v

fromPredicate :: (Word8 -> Bool) -> ByteSet
fromPredicate p = ByteSet $ V.generate 256 (p . toEnum)

instance IsList ByteSet where
    type Item ByteSet = Word8
    toList = map toEnum . V.toList . V.findIndices id . getByteSet
    fromList cs = ByteSet $ V.create $ do
        v <- VM.new 256
        for_ cs $ \c ->
            VM.write v (fromEnum c) True
        return v

instance Show ByteSet where
    showsPrec prec bs@(ByteSet v) = case order bs of
        0 -> showString "zero"
        256 -> showString "one"
        1 -> showParen (prec > 10) $
            let Just b = choose bs
            in showString "singleton " . showsPrec 11 b
        n -> showParen (prec > 10) $ if n > 128
                then showString "complement " . showsPrec 11 (complement bs)
                else showString "fromList " . showsPrec 11 (toList bs)
