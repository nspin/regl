{-# LANGUAGE OverloadedStrings #-}

module Parse
    ( parseRegex
    ) where

import Data.Regex.HeavyDuty

import Control.Applicative hiding (optional)
import Data.Foldable
import Data.Functor
import Data.Word (Word8)
import Data.Char (ord)

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A

parseRegex :: A.Parser (Language Word8)
parseRegex = product_ <$> sequenceA [anchor "^", disj, anchor "$"]
  where
    disj = sum_ <$> conj `A.sepBy` "|"
    conj = foldr intersection (complement_ zero) <$> cat `A.sepBy1` "&"
    cat = product_ <$> many (prefix <*> inner <**> postfix)

anchor :: A.Parser a -> A.Parser (Language Word8)
anchor p = one <$ p <|> pure (complement_ zero)

inner :: A.Parser (Language Word8)
inner = choice
    [ within "("  ")" parseRegex
    , within "[^" "]" $ charClass <&> literal
    , within "["  "]" $ charClass <&> complement_ . literal
    , "." $> literal one
    , "\\" *> (literal . singleton . fromIntegral . ord <$> A.satisfy (`elem` special))
    , A.satisfy (not . (`elem` special)) <&> literal . singleton . fromIntegral . ord
    ]
  where
    special :: String
    special = ".?*+!&|^${}()[]\\"

prefix :: A.Parser (Language Word8 -> Language Word8)
prefix = complement_ <$ "!" <|> pure id

postfix :: A.Parser (Language Word8 -> Language Word8)
postfix = choice
    [ "*" $> star
    , "+" $> plus
    , "?" $> optional
    , within "{" "}" $ choice
        [ countMax   <$> ("," *> A.decimal)
        , countRange <$> (A.decimal <* ",") <*> A.decimal
        , countMin   <$> (A.decimal <* ",")
        , count      <$> A.decimal
        ]
    , pure id
    ]

within :: Applicative f => f bra -> f ket -> f a -> f a
within bra ket v = bra *> v <* ket

choice :: (Alternative f, Foldable t) => t (f a) -> f a
choice = foldr (<|>) empty

charClass :: A.Parser (CharacterClass Word8)
-- charClass = A.inClass . T.unpack <$> A.takeTill (== ']')
charClass = undefined
