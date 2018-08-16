import Regex

import Data.Regex.Poly

import Control.Monad
import System.Environment
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T

main :: IO ()
main = do
    [restr, input] <- getArgs
    display False restr input

display :: Bool -> String -> String -> IO ()
display debug restr input = case A.parseOnly (regex <* A.endOfInput) (T.pack restr) of
    Left err -> error err
    Right re -> do
        when debug $ print re
        case matchList (compile re) input of
            Nothing -> putStrLn "** does not match **"
            Just is -> do
                putStrLn restr
                let go last [] = return ()
                    go last ((i, s):rest) = do
                        dist <-
                            if i > last
                                then return (i - last - 1)
                                else putChar '\n' >> return i
                        putStr (replicate dist ' ')
                        putChar s
                        go i rest
                go (-1) (zip is input)
                putChar '\n'
