{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Criterion.Main (bench, bgroup, defaultMain, nf)

import Data.Regex.Poly
import Data.Regex.Poly.Matcher

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Proxy
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T

main :: IO ()
main = do
    let s = take 1024 . cycle $ ['a'..'z'] ++ ['A'..'Z']
    defaultMain
        [ bench "derivative" $ nf alphas s
        ]
        -- , bench "nfa-like" $ nf (alphas (Proxy :: Proxy (NFALikeRE s))) s

alphas :: [Char] -> Maybe ()
alphas = matchList (compile re)
  where
    re :: RE Char ()
    re = void $ many (satisfy isAlpha)
