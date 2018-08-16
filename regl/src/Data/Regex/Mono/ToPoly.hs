module Data.Regex.Mono.ToPoly
    ( monoToPoly
    ) where

import qualified Data.ByteSet as ByteSet
import qualified Data.Regex.Poly as Poly
import Data.Regex.Mono.Core
import Control.Applicative
import Data.Word

monoToPoly :: RE -> Poly.RE Word8 ()
monoToPoly = go
  where
    go r0 = case r0 of
        Null -> empty
        Epsilon -> pure ()
        Symbol bs -> Poly.generalSymbol $ \s -> if ByteSet.member s bs then Just () else Nothing
        Complement r -> Poly.complement Poly.Greedy () (go r)
        Star r -> Poly.star Poly.Greedy id const () (go r)
        Or rx ry -> go rx <|> go ry
        And rf ra ->  (id <$ go rf) `Poly.intersect` go ra
        Then rf ra -> const <$> go rf <*> go ra
