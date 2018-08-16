{-# LANGUAGE OverloadedStrings #-}

module Regex
    ( regex
    ) where

import Data.Regex.Poly

import Control.Applicative
import Control.Monad.Trans.State
import Data.Foldable
import Data.Functor

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Internal.Types as AI

regex :: A.Parser (RE Char [Int])
regex = disj
  where
    disj = choice <$> conj `sepBy` "|"
    conj = getAndRE . fmap concat . sequenceA . fmap AndRE <$> cat `sepBy1` "&"
    cat = fmap concat . sequenceA <$> many ((prefix <*> inner) <**> postfix)

inner :: A.Parser (RE Char [Int])
inner = choice
    [ between "("  ")" regex
    , between "[^" "]" $ do
        i <- getPos
        cls <- charClass
        return $ [i] <$ satisfy (not . cls)
    , between "["  "]" $ do
        i <- getPos
        cls <- charClass
        return $ [i] <$ satisfy cls
    , "." *> do
        i <- getPos
        return $ [i - 1] <$ dot
    , "\\" *>  do
        i <- getPos
        c <- A.satisfy (`elem` special)
        return $ [i] <$ symbol c
    , do
        i <- getPos
        c <- A.satisfy (not . (`elem` special))
        return $ [i] <$ symbol c
    ]
  where
    special :: String
    special = ".?*+!&|^${}()[]\\"

prefix :: A.Parser (RE Char [Int] -> RE Char [Int])
prefix = go <*> quantifier <|> pure id
  where
    go = do
        "!"
        i <- getPos
        return $ \quant re -> map (const i) <$> justMatch (complement quant () re)

postfix :: A.Parser (RE Char [Int] -> RE Char [Int])
postfix = base <*> quantifier <|> pure id
  where
    base = choice
        [ "*" $> \greed -> fmap concat . withQuantifier greed many
        , "+" $> \greed -> fmap concat . withQuantifier greed some
        , "?" $> \greed -> fmap (maybe [] id) . withQuantifier greed optional
        , between "{" "}" $ choice
            [ "," *> A.decimal <&> \n greed -> fmap concat . withQuantifier greed (countMax n)
            , A.decimal <* "," <&> \n greed -> fmap concat . withQuantifier greed (countMin n)
            , A.decimal <&> \n greed -> fmap concat . count n
            , let f n1 n2 greed = fmap concat . withQuantifier greed (countRange n1 n2)
              in f <$> A.decimal <* "," <*> A.decimal
            ]
        ]

quantifier :: A.Parser Quantifier
quantifier = Reluctant <$ "?" <|> pure Greedy

charClass :: A.Parser (Char -> Bool)
charClass = A.inClass . T.unpack <$> A.takeTill (== ']')

getPos :: A.Parser Int
getPos = AI.Parser $ \t pos more _ succ' -> succ' t pos more (AI.fromPos pos)
