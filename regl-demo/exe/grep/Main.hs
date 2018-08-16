{-# LANGUAGE OverloadedStrings #-}

import Parse

import Data.Regex.Mono
import Data.Regex.DFA

import Data.Conduit
import System.Environment
import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString.Lazy as L
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T

main :: IO ()
main = do
    [restr] <- getArgs
    case A.parseOnly (parseRegex <* A.endOfInput) (T.pack restr) of
        Left err -> error err
        Right re -> runConduit $
               C.stdin
            .| C.map (L.fromChunks . (:[]))
            .| C.linesUnboundedAscii
            .| C.filter (matchLBS (compile re))
            .| C.map (<> "\n")
            .| awaitForever C.sourceLazy
            .| C.stdout
