{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Regex.Poly as Poly hiding (compile)
import qualified Data.Regex.Poly as Poly
import Data.Regex.Mono as Mono
import Data.Regex.DFA
import Data.Regex.DFA.Streaming

import Control.Applicative
import qualified Data.ByteString as B

import Test.QuickCheck
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

main = defaultMain tests

tests = testGroup "tests"
    [ testProperty "abc" abc
    , testProperty "fob" fob
    , testProperty "streem" streem
    ]

abc = matchList (Poly.compile (withMatch re)) input === Just (input, (["a3c","a4c","a5c","a6c"], "bar"))
  where
    re :: Poly.RE Char ([String], String)
    re = (,)
        <$> (symbol 'f' *> few1 abc *> abc *> some abc)
        <*> string "bar"
    abc = sequenceA [symbol 'a', dot, symbol 'c']
    input = "fa1ca2ca3ca4ca5ca6cbar" :: String

fob = match "fobbbbboooooooooooooooooooooo" re === Just ["f", "o", "b", "bbbb", "oooooooooooooooooooooo"]
  where
    re = sequenceA ["f", "o", fmap pure dot, many (symbolIn ("b" :: String)), many dot]

match :: String -> Poly.RE Char a -> Maybe a
match str re = matchList (Poly.compile re) str


streem = testSearch re ip === Just ("abc","foo","oooofooxxxy")
  where
    re = bytes "foo"
    ip = ["abcf", "oooooo", "foox", "xxy"]

testSearch :: Mono.RE -> [B.ByteString] -> Maybe (B.ByteString, B.ByteString, B.ByteString)
testSearch re input = go input (search dfa)
  where
    dfa = compile re
    go cs srch = case srch of
        Found (BufferPartition l m r) -> Just (mconcat l, mconcat m, mconcat $ r ++ cs)
        Partial dropped done more -> case cs of
            [] -> case done of
                Just (BufferPartition l m r) -> Just (mconcat l, mconcat m, mconcat r)
                Nothing -> Nothing
            c:cs' -> prepend dropped <$> go cs' (more c)
    prepend l' (l, m, r) = (mconcat l' <> l, m, r)
