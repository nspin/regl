{-# LANGUAGE OverloadedStrings #-}

import Parse

import Data.Regex.HeavyDuty

import Control.Monad
import Data.Maybe
import System.Environment
import System.IO
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

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

-- test :: String -> String -> Bool
-- test input restr = case A.parseOnly (regex <* A.endOfInput) (T.pack restr) of
--     Left err -> error err
--     Right re -> isJust $ matchList (compile re) input
