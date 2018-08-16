{-# LANGUAGE GADTs #-}

module Data.Regex.Poly.Core
    ( RE(..)
    , Quantifier(..)
    , star
    , generalSymbol
    , complement
    , intersect
    , nullable
    , derivative
    , compile
    , enumerate
    ) where

import Data.Regex.Poly.Matcher

import Control.Applicative
import Data.Profunctor (Profunctor(..))

data RE s a where
    Null :: RE s a
    Epsilon :: a -> RE s a
    Symbol :: (s -> Maybe a) -> RE s a
    Complement :: Quantifier -> a -> RE s b -> RE s a
    Star :: Quantifier -> (x -> b) -> (a -> x -> x) -> x -> RE s a -> RE s b
    Or :: RE s a -> RE s a -> RE s a
    Then :: RE s (a -> b) -> RE s a -> RE s b
    And :: RE s (a -> b) -> RE s a -> RE s b

data Quantifier = Greedy | Reluctant deriving (Eq, Show)

instance Functor (RE s) where
    fmap f r0 = case r0 of
        Null -> Null
        Epsilon a -> Epsilon (f a)
        Symbol p -> Symbol ((fmap . fmap) f p)
        Complement quant a r -> Complement quant (f a) r
        Star quant extract step initial r -> Star quant (f . extract) step initial r
        Or rx ry -> Or (fmap f rx) (fmap f ry)
        And rf ra -> And ((fmap . fmap) f rf) ra
        Then rf ra -> Then ((fmap . fmap) f rf) ra

instance Applicative (RE s) where
    pure = Epsilon

    Null <*> ra = Null
    rf <*> Null = Null
    Epsilon f <*> ra = fmap f ra
    rf <*> Epsilon a = fmap ($ a) rf
    rf <*> ra = Then rf ra

instance Alternative (RE s) where
    empty = Null
    many = star Greedy id (:) []
    some v = (:) <$> v <*> many v

    Null <|> ry = ry
    rx <|> Null = rx
    Epsilon a <|> Epsilon _ = Epsilon a
    Symbol px <|> Symbol py = Symbol ((<|>) <$> px <*> py)
    Complement quant a Null <|> ry = Complement quant a Null
    rx <|> ry = Or rx ry

star :: Quantifier -> (x -> b) -> (a -> x -> x) -> x -> RE s a -> RE s b
star quant extract step initial r0 = case r0 of
    Null -> Null
    Epsilon _ -> Epsilon (extract initial)
    Star _ extract' step' initial' r -> extract . flip step initial <$> star quant extract' step' initial' r -- TODO: is this the most sane choice?
    -- Star Greedy extract' step' initial' r -> extract . flip step initial <$> star quant extract' step' initial' r
    -- Star Reluctant extract' step' initial' r -> star quant extract (\a -> step (extract' (step' a initial'))) initial r
    r -> Star quant extract step initial r

generalSymbol :: (s -> Maybe a) -> RE s a
generalSymbol = Symbol

complement :: Quantifier -> a -> RE s b -> RE s a
complement quant a r0 = case r0 of
    Complement _ b r -> a <$ r -- TODO: what about the quantifier?
    r -> Complement quant a r

intersect :: RE s (a -> b) -> RE s a -> RE s b
intersect Null ra = Null
intersect rf Null = Null
intersect (Complement quant f Null) ra = fmap f ra
intersect rf (Complement quant a Null) = fmap ($ a) rf
intersect rf ra = And rf ra

nullable :: RE s a -> Maybe (Quantifier, a)
nullable r0 = case r0 of
    Null -> Nothing
    Epsilon a -> Just (Reluctant, a)
    Symbol _ -> Nothing
    Complement quant a r -> maybe (Just (quant, a)) (const Nothing) (nullable r)
    Star quant extract _ initial _ -> Just (quant, extract initial)
    Or rx ry -> nullable rx <|> nullable ry
    And rf ra -> q <$> nullable rf <*> nullable ra
    Then rf ra -> q <$> nullable rf <*> nullable ra
  where
    q (qf, f) (qa, a) = (qfa, f a)
      where
        qfa = case (qf, qa) of
            (Reluctant, Reluctant) -> Reluctant
            _ -> Greedy

derivative :: s -> RE s a -> RE s a
derivative s r0 = case r0 of
    Null -> empty
    Epsilon _ -> empty
    Symbol p -> maybe empty pure (p s)
    Complement quant a r -> complement quant a (derivative s r)
    Star quant extract step initial r -> extract <$> (step <$> derivative s r <*> star quant id step initial r)
    Or rx ry -> derivative s rx <|> derivative s ry
    And rf ra -> derivative s rf `intersect` derivative s ra
    Then rf ra -> case nullable rf of
        Nothing -> derivative s rf <*> ra
        Just (Greedy, f) -> derivative s rf <*> ra <|> f <$> derivative s ra
        Just (Reluctant, f) -> f <$> derivative s ra <|> derivative s rf <*> ra

compile :: RE s a -> Matcher s a
compile r = Matcher (snd <$> nullable r) $ \s -> case derivative s r of
    Null -> Nothing
    r' -> Just (compile r')

enumerate :: [s] -> RE s a -> [[s]]
enumerate alphabet = go
  where
    go Null = empty
    go (Epsilon _) = pure []
    go r = maybe id (const ([]:)) (nullable r) $ do
        s <- alphabet
        (s:) <$> go (derivative s r)

-- Boring

instance Profunctor RE where
    rmap = fmap
    lmap g r0 = case r0 of
        Null -> Null
        Epsilon a -> Epsilon a
        Symbol p -> Symbol (p . g)
        Complement quant a r -> Complement quant a (lmap g r)
        Star quant extract step initial r -> Star quant extract step initial (lmap g r)
        Or rx ry -> Or (lmap g rx) (lmap g ry)
        And rf ra -> And (lmap g rf) (lmap g ra)
        Then rf ra -> Then (lmap g rf) (lmap g ra)

instance Show (RE s a) where
    showsPrec prec = go False
      where
        go :: Bool -> RE s b -> ShowS
        go paren r0 = case r0 of
            Null -> showString "empty"
            Epsilon _ -> showParen paren $ showString "pure _"
            Symbol _ -> showParen paren $ showString "generalSymbol _"
            Complement quant _ r -> showParen paren $ showString "Complement _ " . shows quant . showString " _ " . go True r
            Star quant _ _ _ r -> showParen paren $ showString "star " . shows quant . showString " _ _ " . go True r
            Or rx ry -> showParen paren $ go True rx . showString " <|> " . go True ry
            And rf ra -> showParen paren $ go True rf . showString " `intersect` " . go True ra
            Then rf ra -> showParen paren $ go True rf . showString " <*> " . go True ra
