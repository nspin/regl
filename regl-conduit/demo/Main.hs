{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Data.Regex.Poly
import Data.Regex.Poly.ToMono
import Data.Regex.Mono.Pretty
-- import Data.Regex.Mono
import Data.Conduit.Regex

import Data.Functor
import Data.Conduit
import Data.Foldable
import Data.Word
import Control.Applicative
import qualified Data.Conduit.Combinators as C
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

main :: IO ()
main = do
    -- runConduitRes $ source .| findAll re .| C.map (\a -> BC.pack (show a ++ "\n")) .| C.stdout
    runConduitRes $ source .| replaceAll re .| C.stdout
  where
    source = C.stdin
    -- source = C.sourceFile "regl-conduit/demo/shakespeare.txt"
    -- show_ = C.map (\a -> BC.pack (show a ++ "\n"))
    -- showLBSs = C.map (\match -> BC.pack (show match ++ "\n"))

-- re = (complement_ Reluctant (anything_ Reluctant *> bytes "\n" <* anything_ Reluctant) &&> ((,,) <$> (B.pack <$> justMatch (anything_ Reluctant)) <*> bytes "Romeo i" <*> (B.pack <$> justMatch (anything_ Reluctant)))) <* bytes "\n"

re = many1 (symbolIn (B.unpack (BC.pack "0123456789"))) <&> \digits -> BL.pack (reverse (toList digits))

bytes :: B.ByteString -> RE Word8 B.ByteString
bytes = fmap B.pack . traverse symbol . B.unpack

-- STALLS:
-- matchList (compile re) $ B.unpack $ BC.pack $ "  Cap. Young Romeo is it?\n"
-- re = (few (complement_ Reluctant (bytes "\n")) &&> (anything_ Reluctant *> bytes "Romeo i" <* anything_ Greedy)) <* bytes "\n"
