-- [lifted from https://github.com/cacay/regexp]
--
-- Copyright (c) 2018 Cosku Acay
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

-- | Definition of Boolean algebras. We base the definition on semirings
-- so it works nicely with "Data.KleeneAlgebra".
module Data.BooleanAlgebra
    ( BooleanAlgebra(..)
    -- * Operations
    , implies
    , butNot
    , ands
    , ors
    ) where

import Data.Semiring (Semiring (..))


-- | A Boolean algebra is a distributive commutative idempotent semiring with
-- complement satisfying some extra equations. More concretely, on top of the
-- semiring axioms, the following axioms need to hold:
--
-- == Distributivity of '<+>' over '<.>'
-- @a '<+>' (b '<.>' c) = (a '<+>' b) '<.>' (a '<+>' c)@
--
-- == Commutativity of '<.>'
-- @a '<.>' b = b '<.>' a@
--
-- == Idempotence of '<+>' and '<.>'
-- @a '<+>' a = a@
--
-- @a '<.>' a = a@
--
-- == Upper bounded by 'one'
-- @a '<+>' 'one' = 'one'@
--
-- == Properties of 'complement'
-- @a '<+>' 'complement' a = 'one'@
--
-- @a '<.>' 'complement' a = 'zero'@
class Semiring a => BooleanAlgebra a where
    -- | Complement or negation.
    complement :: a -> a


-- | Booleans form a boolean algebra.
instance BooleanAlgebra Bool where
    complement = not


-- * Functions over Boolean algebras

-- | @p `implies` q@ holds if @p@ holds implies @q@ holds.
implies :: BooleanAlgebra a => a -> a -> a
implies p q =
    complement p <+> q


-- | @p `butNot` q@ holds when @p@ holds but @q@ doesn't.
butNot :: BooleanAlgebra a => a -> a -> a
butNot p q =
    p <.> complement q


-- | Logical conjunction of all elements in a container.
ands :: (BooleanAlgebra a, Foldable t) => t a -> a
ands t =
    foldr (<.>) one t


-- | Logical disjunction of all elements in a container.
ors :: (BooleanAlgebra a, Foldable t) => t a -> a
ors =
    foldr (<+>) zero
