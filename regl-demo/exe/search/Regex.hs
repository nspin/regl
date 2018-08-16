{-# LANGUAGE OverloadedStrings #-}

module Regex
    ( regex
    ) where

import Data.Regex.Poly

import Control.Applicative
import Data.Foldable
import Data.Functor

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A

regex :: A.Parser (RE Char ())
regex = sequenceA_ <$> sequenceA [anchor "^", disj, anchor "$"]
  where
    disj = choice <$> conj `sepBy` "|"
    conj = getAndRE . sequenceA_ . fmap AndRE <$> cat `sepBy1` "&"
    cat = sequenceA_ <$> many ((prefix <*> inner) <**> postfix)

anchor :: A.Parser a -> A.Parser (RE Char ())
anchor p = pure () <$ p <|> pure (void $ few dot)

inner :: A.Parser (RE Char ())
inner = choice
    [ between "("  ")" regex
    , between "[^" "]" $ charClass <&> void . satisfy . (not .)
    , between "["  "]" $ charClass <&> void . satisfy
    , "." $> void dot
    , "\\" *> (void . symbol <$> A.satisfy (`elem` special))
    , A.satisfy (not . (`elem` special)) <&> void . symbol
    ]
  where
    special :: String
    special = ".?*+!&|^${}()[]\\"

prefix :: A.Parser (RE Char () -> RE Char ())
prefix = (flip complement () <$ "!") <*> quantifier <|> pure id

postfix :: A.Parser (RE Char () -> RE Char ())
postfix = base <*> quantifier <|> pure id
  where
    base = choice
        [ "*" $> \greed -> void . withQuantifier greed many
        , "+" $> \greed -> void . withQuantifier greed some
        , "?" $> \greed -> void . withQuantifier greed optional
        , between "{" "}" $ choice
            [ "," *> A.decimal <&> \n greed -> void . withQuantifier greed (countMax n)
            , A.decimal <* "," <&> \n greed -> void . withQuantifier greed (countMin n)
            , A.decimal <&> \n greed -> count_ n
            , let f n1 n2 greed = void . withQuantifier greed (countRange n1 n2)
              in f <$> A.decimal <* "," <*> A.decimal
            ]
        ]

quantifier :: A.Parser Quantifier
quantifier = Greedy <$ "?" <|> pure Reluctant

charClass :: A.Parser (Char -> Bool)
charClass = A.inClass . T.unpack <$> A.takeTill (== ']')
