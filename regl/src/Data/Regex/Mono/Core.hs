{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Regex.Mono.Core
    ( RE(..)

    , not_
    , and_
    , literal

    , isCertainlyZero
    
    , nullable
    , derivative
    , partitionDerivatives
    , transitions
    , uniqueTransitions

    , backwards

    , enumerate
    ) where

import Data.ByteSet

import Data.Semiring
import Data.BooleanAlgebra
import Data.KleeneAlgebra

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData)
import Data.Bool (bool)
import Data.Tuple (swap)
import Data.Word (Word8)
import GHC.Generics (Generic)
import qualified Data.Map as M

data RE = 
      Null
    | Epsilon
    | Symbol ByteSet
    | Complement RE
    | Star RE
    | Or RE RE
    | And RE RE
    | Then RE RE
    deriving (Eq, Ord, Generic, NFData)

instance Semiring RE where
    zero = Null
    one  = Epsilon

    Null <+> r = r
    Complement Null <+> r = Complement Null
    Symbol x <+> Symbol y = Symbol (x <+> y)
    r <+> (Or s t) = (r <+> s) <+> t
    r <+> s = case compare r s of
        EQ -> r
        LT -> Or r s
        GT -> s <+> r

    r <.> Null = Null
    Null <.> r = Null
    r <.> Epsilon = r
    Epsilon <.> r = r
    r <.> (Then s t) = (r <.> s) <.> t
    r <.> s = Then r s

-- TODO: Is there a way to guarantee that this terminates?
-- TODO: Reason more about normalization with respect to 'Null'.
-- instance DetectableZero RE where
--     isZero r = case enumerate r of
--         [] -> True
--         _ -> False

isCertainlyZero :: RE -> Bool
isCertainlyZero Null = True
isCertainlyZero _ = False

instance KleeneAlgebra RE where
    star Null = Epsilon
    star Epsilon = Epsilon
    star (Star r) = star r
    star r = Star r

literal :: ByteSet -> RE
literal cc = if isZero cc then Null else Symbol cc

not_ :: RE -> RE
not_ (Complement r) = r
not_ (Symbol x) = Symbol (complement x)
not_ r = Complement r

and_ :: RE -> RE -> RE
and_ Null r = Null
and_ (Complement Null) r = r
and_ r (And s t) = and_ (and_ r s) t
and_ (Symbol x) (Symbol y) = Symbol (x <.> y)
and_ r s = case compare r s of
    EQ -> r
    LT -> And r s
    GT -> and_ s r

nullable :: RE -> Bool
nullable r0 = case r0 of
    Null         -> False
    Epsilon      -> True
    Symbol _     -> False
    Complement r -> complement (nullable r)
    Star _       -> True
    Or r s       -> nullable r <+> nullable s
    And r s      -> nullable r <.> nullable s
    Then r s     -> nullable r <.> nullable s

derivative :: Word8 -> RE -> RE
derivative c = go
  where
    go r0 = case r0 of
        Null         -> zero
        Epsilon      -> zero
        Symbol v     -> bool zero one (member c v)
        Complement r -> not_ (go r)
        Star r       -> go r <.> star r
        Or r s       -> go r <+> go s
        And r s      -> go r `and_` go s
        Then r s     -> go r <.> s <+> bool zero (go s) (nullable r)

-- all nonempty
partitionDerivatives :: RE -> [ByteSet]
partitionDerivatives = go
  where
    cross = liftA2 (<.>)
    go r0 = filter (not . isZero) $ case r0 of
        Null         -> [one]
        Epsilon      -> [one]
        Symbol cc    -> [cc, complement cc]
        Complement r -> go r
        Star r       -> go r
        Or r s       -> cross (go r) (go s)
        And r s      -> cross (go r) (go s)
        Then r s     -> bool id (cross (go s)) (nullable r) (go r)

-- regexes not necessarily distinct
transitions :: RE -> [(ByteSet, RE)]
transitions r = do
    partition <- partitionDerivatives r
    Just c <- [choose partition]
    return (partition, derivative c r)

uniqueTransitions :: RE -> M.Map RE ByteSet
uniqueTransitions = M.fromListWith (<+>) . map swap . transitions 

backwards :: RE -> RE
backwards = go
  where
    go r0 = case r0 of
        Null         -> r0
        Epsilon      -> r0
        Symbol cc    -> r0
        Complement r -> Complement (go r)
        Star r       -> Star (go r)
        Or r s       -> Or (go r) (go r)
        And r s      -> And (go r) (go r)
        Then r s     -> Then (go s) (go r)

enumerate :: RE -> [[Word8]]
enumerate = go
  where
    go Null = []
    go Epsilon = [[]]
    go r = bool id ([]:) (nullable r) $ do
        c <- [0..255]
        (c:) <$> go (derivative c r)

-- Boring

instance Show RE where
    showsPrec = goWith True
      where
        go = goWith False
        goWith isFirst prec r0 = case r0 of
            Null         -> showString "zero"
            Epsilon      -> showString "one"
            Symbol bs    -> showParen (prec > 10) $ showString "literal " . showsPrec 11 bs
            Complement r -> showParen (prec > 10) $ showString "not_ " . go 11 r
            Star r       -> showParen (prec > 10) $ showString "star " . go 11 r
            Or r s       -> paren 6 $ go 7 r . showString " <+> " . go 7 s
            And r s      -> showParen (prec > 10) $ showString "and_ " . go 11 r . showString " " . go 11 s
            Then r s     -> paren 7 $ go 8 r . showString " <.> " . go 8 s
          where
            paren p = showParen $ if isFirst then prec > p else prec > p + 1

-- instance Show RE where
--     showsPrec prec r0 = case r0 of
--         Null         -> showString "zero"
--         Epsilon      -> showString "one"
--         Symbol bs    -> showParen (prec > 10) $ showString "literal " . showsPrec 11 bs
--         Complement r -> showParen (prec > 10) $ showString "not_ " . showsPrec 11 r
--         Star r       -> showParen (prec > 10) $ showString "star " . showsPrec 11 r
--         Or r s       -> showParen (prec >  6) $ showsPrec 7 r . showString " <+> " . showsPrec 7 s
--         And r s      -> showParen (prec > 10) $ showString "and_ " . showsPrec 11 r . showString " " . showsPrec 11 s
--         Then r s     -> showParen (prec >  7) $ showsPrec 8 r . showString " <.> " . showsPrec 8 s
