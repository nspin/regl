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

-- | Definition of Kleene algebras.
module Data.KleeneAlgebra
    ( KleeneAlgebra(..)
    ) where

import Data.Semiring (Semiring (..))


-- | A Kleene algebra is an /idempotent/ semiring with an additional operation
-- called the Kleene star. In addition to the semiring axioms, a Kleene algebra
-- needs to satisfy the following properties:
--
-- == Idempotence of '<+>'
-- @a '<+>' a = a@
--
-- == Properties of 'star'
-- @'one' '<+>' (a '<.>' 'star' a) <= 'star' a@
--
-- @'one' '<+>' ('star' a '<.>' a) <= 'star' a@
--
-- @b '<+>' (a '<.>' x) <= x ==> ('star' a) '<.>' b <= x@
--
-- @b '<+>' (x '<.>' a) <= x ==> b '<.>' ('star' a) <= x@
--
-- Here, @a <= b@ is defined as @a '<+>' b = b@.
class Semiring a => KleeneAlgebra a where
    -- | Kleene star. Captures the notion of /iteration/.
    star :: a -> a


-- | Booleans form a (trivial) Kleene algebra.
instance KleeneAlgebra Bool where
    star _ =
        True
