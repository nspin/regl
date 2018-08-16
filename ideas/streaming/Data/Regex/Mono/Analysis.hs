module Data.Regex.Mono.Analysis
    ( lengthBound
    ) where

import Data.ByteSet
import Data.Regex.Mono.Core
import Data.Regex.Internal.DistanceBound

lengthBound :: RE -> DistanceBound
lengthBound = go
  where
    go r = case r of
        Null         -> 0
        Epsilon      -> 0
        Symbol cc    -> 1
        Complement r -> unbounded
        Star r       -> unbounded
        Or r s       -> go r `max` go s
        And r s      -> go r `min` go s
        Then r s     -> go s + go r
