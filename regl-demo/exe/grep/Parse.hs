{-# LANGUAGE OverloadedStrings #-}

module Parse
    ( parseRegex
    ) where

import Data.Regex.Mono

import Control.Applicative hiding (optional)
import Data.Char (ord)
import Data.Functor
import qualified Data.Attoparsec.Text as A

parseRegex :: A.Parser RE
parseRegex = product_ <$> sequenceA [anchor "^", disj, anchor "$"]
  where
    disj = sum_ <$> conj `A.sepBy` "|"
    conj = foldr and_ (not_ zero) <$> cat `A.sepBy1` "&"
    cat = product_ <$> many (prefix <*> inner <**> postfix)

anchor :: A.Parser a -> A.Parser RE
anchor p = one <$ p <|> pure (not_ zero)

inner :: A.Parser RE
inner = choice
    [ within "("  ")" parseRegex
    , within "[^" "]" $ charClass <&> literal
    , within "["  "]" $ charClass <&> not_ . literal
    , "." $> literal one
    , "\\" *> (literal . singleton . fromIntegral . ord <$> A.satisfy (`elem` special))
    , A.satisfy (not . (`elem` special)) <&> literal . singleton . fromIntegral . ord
    ]
  where
    special :: String
    special = ".?*+!&|^${}()[]\\"

prefix :: A.Parser (RE -> RE)
prefix = not_ <$ "!" <|> pure id

postfix :: A.Parser (RE -> RE)
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

charClass :: A.Parser ByteSet
charClass = undefined -- TODO
