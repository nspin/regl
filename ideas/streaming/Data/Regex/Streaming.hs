module Data.Regex.Streaming
    ( Search(..)
    , search
    ) where

import Data.Regex.Mono as Mono
import Data.Regex.DFA
import Data.Regex.DFA.Analysis
import Data.Regex.Internal.DistanceBound
import Data.Regex.Streaming.HFA

import Foreign (peek, plusPtr, withForeignPtr)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as BI

import Control.Monad.Primitive

data Search m =
    Partial (B.ByteString -> m (Search m))
  | Found [B.ByteString] [B.ByteString] [B.ByteString]

search :: PrimMonad m => Mono.RE -> B.ByteString -> m (Search m)
search re chunk0 = undefined
-- search re chunk0 = do
--     hfa <- hfaCreate dfawdb
--     let go acc nacc state chunk =
--             let n = B.length chunk
--                 step s i
--                     | dfaAccept dfa s = return $
--                         let (yes, no) = B.splitAt i chunk
--                             pets []      cur post s' i' best = best
--                             pets (p:pre) cur post s' i' best
--                                 | dfaAccept afd s' =
--                                     pets (p:pre) cur post (dfaStep afd s' (B.index cur i')) (i' + 1)
--                                 | i' == B.length cur = pets pre p (cur:post) s' 0
--                                 | otherwise =
--                                     pets (p:pre) cur post (dfaStep afd s' (B.index cur i')) (i' + 1)

--                             (before, during) = pets (yes:acc) [] 0 0 undefined
--                         in Found before during [no]
--                     | i == n = return $
--                         let bound = dfaDistanceBound dfawdb s
--                         in Partial $ go (chunk:acc) (n + nacc) s
--                     | otherwise = do
--                         step (dfaStep dfa s (B.index chunk i)) (i + 1)
--             in step state 0
--     go [] 0 0 chunk0
--   where
--     dfa = compile re
--     dfawdb = withDistanceBounds dfa
--     afd = compile er
--     er = Mono.backwards re
