module Data.Regex.Poly.Matcher
    ( Matcher(..)
    , matchList
    ) where

import Data.Profunctor (Profunctor(..))

data Matcher s a = Matcher (Maybe a) (s -> Maybe (Matcher s a))

instance Functor (Matcher s) where
    fmap = rmap

instance Profunctor Matcher where
    dimap g f (Matcher mr step) = Matcher (fmap f mr) ((fmap . fmap) (dimap g f) (lmap g step))

matchList :: Matcher s a -> [s] -> Maybe a
matchList (Matcher mr _) [] = mr
matchList (Matcher _ step) (s:ss) = step s >>= flip matchList ss

-- matchStream :: RE re => re a -> [Alphabet re] -> Maybe [a]
-- matchStream r = matchSimple $ many (few dot *> r) <* many dot
