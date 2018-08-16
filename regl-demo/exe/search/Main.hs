import Regex

import Data.Regex.Poly

import Control.Monad
import Data.Maybe
import System.Environment
import System.IO
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
    [restr] <- getArgs
    case A.parseOnly (regex <* A.endOfInput) (T.pack restr) of
        Left err -> error err
        Right re ->
            let matcher = compile re
                go = do
                    eof <- hIsEOF stdin
                    unless eof $ do
                        line <- T.getLine
                        case matchList matcher (T.unpack line) of
                            Nothing -> return ()
                            Just _ -> T.putStrLn line
                        go
            in go

test :: String -> String -> Bool
test input restr = case A.parseOnly (regex <* A.endOfInput) (T.pack restr) of
    Left err -> error err
    Right re -> isJust $ matchList (compile re) input
