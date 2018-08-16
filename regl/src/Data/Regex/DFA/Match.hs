module Data.Regex.DFA.Match
    ( matchBS
    , fullMatchBS
    , matchLBS
    , fullMatchLBS
    ) where

import Data.Regex.DFA.Core

import Foreign (peek, plusPtr, withForeignPtr)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as BI

matchBS :: DFA -> B.ByteString -> Bool
matchBS dfa = either id (const False) . matchBSFrom 0 dfa

fullMatchBS :: DFA -> B.ByteString -> Bool
fullMatchBS dfa = maybe False (dfaAccept dfa) . fullMatchBSFrom 0 dfa

matchBSFrom :: DFAState -> DFA -> B.ByteString -> Either Bool DFAState
matchBSFrom start dfa (BI.PS fp off len) = case trivialityView dfa of
    Trivial r -> Left r
    NonTrivial reject ->
        let go s p q
                | dfaAccept dfa s = return $ Left True
                | s == reject     = return $ Left False
                | p == q          = return $ Right s
                | otherwise       = do
                    c <- peek p
                    go (dfaStep dfa s c) (p `plusPtr` 1) q
        in BI.accursedUnutterablePerformIO $ withForeignPtr fp $ \p ->
            go start (p `plusPtr` off) (p `plusPtr` (off + len)) 

fullMatchBSFrom :: DFAState -> DFA -> B.ByteString -> Maybe DFAState
fullMatchBSFrom start dfa (BI.PS fp off len) = case trivialityView dfa of
    Trivial r -> Nothing
    NonTrivial reject ->
        let go s p q
                | s == reject     = return Nothing
                | p == q          = return $ Just s
                | otherwise       = do
                    c <- peek p
                    go (dfaStep dfa s c) (p `plusPtr` 1) q
        in BI.accursedUnutterablePerformIO $ withForeignPtr fp $ \p ->
            go start (p `plusPtr` off) (p `plusPtr` (off + len)) 

matchLBS :: DFA -> L.ByteString -> Bool
matchLBS dfa = go 0 . L.toChunks
  where
    go s [] = dfaAccept dfa s
    go s (c:cs) = case matchBSFrom s dfa c of
        Left r -> r
        Right s' -> go s' cs

fullMatchLBS :: DFA -> L.ByteString -> Bool
fullMatchLBS dfa = go 0 . L.toChunks
  where
    go s [] = dfaAccept dfa s
    go s (c:cs) = case fullMatchBSFrom s dfa c of
        Nothing -> False
        Just s' -> go s' cs
