module Data.Regex.DFA.Streaming.Buffer
    ( Buffer
    , Offset
    , Length
    , length
    , startOffset
    , endOffset
    , toChunks
    , initial
    , add
    , splitAt
    ) where

import Prelude hiding (length, splitAt)
import qualified Data.ByteString as B

type Offset = Int
type Length = Int

data Buffer = Buffer
    { bufferStart :: Offset
    , bufferLength :: Length
    , bufferChunks :: [B.ByteString]
    } deriving Show

length :: Buffer -> Length
length (Buffer _ n _) = n

startOffset :: Buffer -> Offset
startOffset (Buffer i _ _) = i

endOffset :: Buffer -> Offset
endOffset (Buffer i n _) = i + n

toChunks :: Buffer -> [B.ByteString]
toChunks (Buffer _ _ chunks) = chunks

initial :: Buffer
initial = Buffer 0 0 []

add :: Buffer -> B.ByteString -> Buffer
add (Buffer i n chunks) chunk = Buffer i (n + B.length chunk) (chunks ++ [chunk])

splitAt :: Offset -> Buffer -> (Buffer, Buffer)
splitAt i' (Buffer i n chunks) = (Buffer i nl chunksl, Buffer i' nr chunksr)
  where
    nl = i' - i
    nr = n - nl
    (chunksl, chunksr) = go nl chunks
    go 0 cs = ([], cs)
    go m (c:cs) = case compare m (B.length c) of
        EQ -> ([c], cs)
        LT ->
            let (cl, cr) = B.splitAt m c
            in ([cl], cr:cs)
        GT -> 
            let (csl, csr) = go (m - B.length c) cs
            in (c:csl, csr)

-- splitAround :: Offset -> Offset -> Buffer -> SplitAround
-- splitAround start end buffer = SplitAround l m r
--   where
--     (l, mr) = splitAt start
--     (m,  r) = splitAt end

-- data BufferSplitAround = BufferSplitAround
--     { bufferL :: Buffer
--     , bufferM :: Buffer
--     , bufferR :: Buffer
--     }
