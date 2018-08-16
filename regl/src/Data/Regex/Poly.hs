module Data.Regex.Poly
    ( RE
    , Quantifier(..)
    , star
    , generalSymbol
    , complement
    , intersect
    , nullable
    , derivative
    , compile
    , enumerate

    , module Data.Regex.Poly.Matcher
    , module Data.Regex.Poly.Combinators
    , module Data.Regex.Poly.Combinators.Char
    , module Data.Regex.Poly.Combinators.General
    ) where

import Data.Regex.Poly.Core
import Data.Regex.Poly.Matcher
import Data.Regex.Poly.Combinators
import Data.Regex.Poly.Combinators.Char
import Data.Regex.Poly.Combinators.General
