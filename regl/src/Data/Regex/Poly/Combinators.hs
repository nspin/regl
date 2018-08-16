{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}

module Data.Regex.Poly.Combinators
    ( dot
    , satisfy
    , symbol
    , symbolIn

    , (<&&), (&&>)
    , isolate
    , isolateMax
    , withMatch
    , justMatch
    , few
    , few1
    , complement_
    , anything
    , anything_

    , AndRE(..)

    , reluctant
    , withQuantifier
    , ReluctantRE(..)
    , orWith
    ) where

import Data.Regex.Poly.Core
import Data.Regex.Poly.Combinators.General

import Control.Applicative
import Data.List.NonEmpty (NonEmpty(..), toList)


(<&&) :: RE s a -> RE s b -> RE s a
(<&&) = intersect . fmap const

(&&>) :: RE s a -> RE s b -> RE s b
(&&>) = flip (<&&)

isolate :: Integral n => n -> RE s a -> RE s a
isolate n r = r <&& count_ n dot

isolateMax :: Integral n => n -> RE s a -> RE s a
isolateMax n r = r <&& countMax_ n dot

withMatch :: RE s a -> RE s ([s], a)
withMatch = intersect $ (,) <$> many dot

justMatch :: RE s a -> RE s [s]
justMatch r = r &&> many dot

few :: RE s a -> RE s [a]
few = star Reluctant id (:) []

few1 :: RE s a -> RE s (NonEmpty a)
few1 r = (:|) <$> r <*> few r

complement_ :: Quantifier -> RE s a -> RE s ()
complement_ quant = complement quant ()

anything :: Quantifier -> a -> RE s a
anything quant a = complement quant a empty

anything_ :: Quantifier -> RE s ()
anything_ quant = anything quant ()

newtype AndRE s a = AndRE { getAndRE :: RE s a }
    deriving (Functor, Alternative)

instance Applicative (AndRE s) where
    pure = AndRE . anything Reluctant
    AndRE rf <*> AndRE ra = AndRE $ intersect rf ra


-- Symbols

dot :: RE s s
dot = generalSymbol Just

satisfy :: (s -> Bool) -> RE s s
satisfy f = generalSymbol $ \s -> if f s then Just s else Nothing

symbol :: Eq s => s -> RE s s
symbol = satisfy . (==)

symbolIn :: (Foldable t, Eq s) => t s -> RE s s
symbolIn = satisfy . flip elem


-- Quantifier

reluctant :: (ReluctantRE s a -> ReluctantRE s b) -> RE s a -> RE s b
reluctant c = unReluctantRE . c . ReluctantRE

withQuantifier :: Quantifier -> (forall f. Alternative f => f a -> f b) -> RE s a -> RE s b
withQuantifier Greedy = id
withQuantifier Reluctant = reluctant

newtype ReluctantRE s a = ReluctantRE { unReluctantRE :: RE s a }
    deriving (Functor, Applicative)

instance Alternative (ReluctantRE s) where
    empty = ReluctantRE empty
    rx <|> ry = ReluctantRE $ unReluctantRE ry <|> unReluctantRE rx
    many = ReluctantRE . few . unReluctantRE
    some = ReluctantRE . fmap toList . few1 . unReluctantRE

orWith :: Alternative f => Quantifier -> f a -> f a -> f a
orWith Greedy = (<|>)
orWith Reluctant = flip (<|>)
