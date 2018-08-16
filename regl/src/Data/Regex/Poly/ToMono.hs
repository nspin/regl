module Data.Regex.Poly.ToMono
    ( polyToMono
    ) where

import qualified Data.ByteSet as ByteSet
import Data.Regex.Poly.Core hiding (star)
import qualified Data.Regex.Mono as Mono
import Data.Word
import Data.Semiring
import Data.KleeneAlgebra

polyToMono :: RE Word8 a -> Mono.RE
polyToMono = go
  where
    go :: RE Word8 a -> Mono.RE
    go r0 = case r0 of
        Null -> zero
        Epsilon _ -> one
        Symbol p -> Mono.literal (ByteSet.fromPredicate (maybe False (const True) . p))
        Complement _ _ r -> Mono.not_ (go r)
        Star _ _ _ _ r -> star (go r)
        Or rx ry -> go rx <+> go ry
        And rf ra -> go rf `Mono.and_` go ra
        Then rf ra -> go rf <.> go ra
