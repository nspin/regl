{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Regex.DFA.Streaming
    ( search
    , Search(..)
    , BufferPartition(..)
    ) where

import Data.Regex.DFA
import Data.Regex.DFA.Streaming.Buffer (Buffer, Offset)
import qualified Data.Regex.DFA.Streaming.Buffer as Buf

import Control.Applicative
import Control.Monad.ST
import Data.Functor
import Data.Word
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed.Mutable as V


data Search =
    Partial [B.ByteString] (Either [B.ByteString] BufferPartition) (B.ByteString -> Search)
  | Found BufferPartition

search :: DFA -> Search
search dfa = case trivialityView dfa of
    Trivial True -> trivialTrueSearch
    Trivial False -> trivialFalseSearch
    NonTrivial reject -> searchNonTrivial dfa reject

trivialTrueSearch :: Search
trivialTrueSearch = Partial [] (Right (BufferPartition [] [] [])) (go [])
  where
    go acc chunk =
        let acc' = acc ++ [chunk]
        in Partial [] (Right (BufferPartition [] acc' [])) (go acc')

trivialFalseSearch :: Search
trivialFalseSearch = Partial [] (Left []) go
  where
    go chunk = Partial [chunk] (Left []) go

searchNonTrivial :: DFA -> DFAState -> Search
searchNonTrivial dfa reject = Partial [] done0 more0
  where
    buffer0 = Buf.initial
    state0 = initialState dfa
    done0 = case extract state0 of
        Nothing -> Left []
        Just (start, end) -> Right (splitAround start end buffer0)
    more0 = go buffer0 state0 0 0

    go buffer state offsetOfChunk offsetInChunk chunk =
        if offsetInChunk == n
        then
            let
                buffer' = buffer `Buf.add` chunk
                (bufl, bufr) = Buf.splitAt (firstOffset state) buffer'
                done = case extract state of
                    Nothing -> Left (Buf.toChunks buffer)
                    Just (start, end) -> Right (splitAround start end buffer)
                more = go bufr state (offsetOfChunk + offsetInChunk) 0
            in Partial (Buf.toChunks bufl) done more
        else
            -- let x = advance dfa reject (chunk `B.index` offsetInChunk) offset state
            -- in  traceShowLn state $
            --     traceShowLn offset $
            --     traceShowLn (chr (fromEnum (chunk `B.index` offsetInChunk))) $
            --     traceShowLn x $
            --     traceShowLn "---------" $
            --     case x of
            case advance dfa reject (chunk `B.index` offsetInChunk) offset state of
                Left (start, end) -> Found $ splitAround start end (buffer `Buf.add` chunk)
                Right state' -> go buffer state' offsetOfChunk (offsetInChunk + 1) chunk
      where
        n = B.length chunk
        offset = offsetOfChunk + offsetInChunk


data SearchState = SearchState
    { searchThreads :: [Thread]
    , searchAccept :: Maybe (Offset, Offset)
    } deriving Show

data Thread = Thread
    { threadStart :: Offset
    , threadState :: DFAState
    } deriving Show

initialState :: DFA -> SearchState
initialState dfa = SearchState
    [Thread 0 dfaStart] 
    (if dfaAccept dfa dfaStart then Just (0, 0) else Nothing)

firstOffset :: SearchState -> Offset
firstOffset (SearchState (thread:_) _) = threadStart thread

extract :: SearchState -> Maybe (Offset, Offset)
extract (SearchState _ accepted) = accepted

advance :: DFA -> DFAState -> Word8 -> Offset -> SearchState -> Either (Offset, Offset) SearchState
advance dfa reject c offset (SearchState threads accepted) = runST $ do
    set <- V.new (dfaSize dfa)
    let go [] = return (Nothing, [])
        go (Thread start state:rest) = do
            let state' = dfaStep dfa state c
                thread' = Thread start state'
            if dfaAccept dfa state'
                then do
                    return (Just (start, offset'), [thread'])
                else do
                    if state' == reject
                        then do
                            go rest
                        else do
                            seen <- V.read set state'
                            if seen
                                then do
                                    go rest
                                else do
                                    V.write set state' True
                                    (fmap . fmap) (thread':) (go rest)
    (acceptedUpdate, threads') <- go threads
    let accepted' = acceptedUpdate <|> accepted
    case accepted' of
        Nothing -> do
            haveStart <- V.read set dfaStart
            return $ Right $
                if haveStart
                then SearchState threads' accepted'
                else SearchState
                    (threads' ++ [Thread offset' dfaStart])
                    (if dfaAccept dfa dfaStart then Just (offset', offset') else Nothing)
        Just range -> do
            return $
                case threads' of
                    [] -> Left range
                    _  -> Right $ SearchState threads' accepted'
  where
    offset' = offset + 1


data BufferPartition = BufferPartition
    { bufferL :: [B.ByteString]
    , bufferM :: [B.ByteString]
    , bufferR :: [B.ByteString]
    }

splitAround :: Offset -> Offset -> Buffer -> BufferPartition
splitAround start end buffer = BufferPartition (Buf.toChunks l) (Buf.toChunks m) (Buf.toChunks r)
  where
    (l, mr) = Buf.splitAt start buffer
    (m,  r) = Buf.splitAt end mr

-- import Data.Char
-- import Data.Ix
-- import Data.List
-- import Data.Monoid
-- import Debug.Trace
-- import qualified Data.Array as A
-- import qualified Data.Array.ST as A
-- import qualified Data.Array.Unsafe as A
-- import qualified Data.ByteString.Internal as BI
-- import qualified Data.ByteString.Lazy as L
-- import qualified Data.Vector as V

-- traceShowLn :: Show a => a -> b -> b
-- traceShowLn a b = trace ("<" ++ show a ++ ">") b

-- traceShowLnM :: (Applicative f, Show a) => a -> f ()
-- traceShowLnM a = traceM ("<" ++ show a ++ ">")
