module Data.Conduit.Regex
    ( findFirstMono
    , findAllMono
    , findFirst
    , findAll
    , replaceFirstMono
    , replaceAllMono
    , replaceFirst
    , replaceAll
    ) where

-- import Data.Regex.Poly as Poly hiding (compile)
import qualified Data.Regex.Poly as Poly
import Data.Regex.Poly.ToMono
import qualified Data.Regex.Poly.Matcher as Poly
import qualified Data.Regex.Mono as Mono
import qualified Data.Regex.DFA as DFA
import qualified Data.Regex.DFA.Streaming as DFA
import Data.Regex.DFA.Streaming

import Data.Conduit
import Control.Monad
import Data.Foldable
-- import Control.Monad.IO.Class
import Data.Functor
import Data.Conduit.Combinators (awaitNonNull)
import Data.Maybe
import Data.NonNull (toNullable)
import qualified Data.Conduit.Combinators as C
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

findFirstMono :: Monad m => Mono.RE -> ConduitT B.ByteString o m (Maybe L.ByteString)
findFirstMono re = go (search (DFA.compile re))
  where
    go (Found (BufferPartition _ match _)) = return (Just (L.fromChunks match))
    go (Partial _ done more) = do
        mchunk <- awaitNonNull
        case mchunk of
            Nothing -> return $ case done of
                Left buffer -> Nothing
                Right (BufferPartition _ match _) -> Just (L.fromChunks match)
            Just chunk -> go (more (toNullable chunk))

findAllMono :: Monad m => Mono.RE -> ConduitT B.ByteString L.ByteString m ()
findAllMono re = go search0
  where
    search0 = search (DFA.compile re)
    go (Found (BufferPartition _ match leftover)) = do
        yield (L.fromChunks match)
        (traverse yield leftover >> awaitForever yield) .| go search0
    go (Partial _ done more) = do
        mchunk <- awaitNonNull
        case mchunk of
            Nothing -> case done of
                Left buffer -> return ()
                Right (BufferPartition _ match leftover) -> do
                    yield (L.fromChunks match)
                    when (not (null leftover)) $
                        traverse_ yield leftover .| go search0
            Just chunk -> go (more (toNullable chunk))

unsafeMatch :: Poly.RE Word8 a -> L.ByteString -> a
unsafeMatch re lbs = fromJust $ Poly.matchList (Poly.compile re) (concatMap B.unpack (L.toChunks lbs))

findFirst :: Monad m => Poly.RE Word8 a -> ConduitT B.ByteString o m (Maybe a)
findFirst re = (fmap . fmap) (unsafeMatch re) (findFirstMono (polyToMono re))

findAll :: Monad m => Poly.RE Word8 a -> ConduitT B.ByteString a m ()
findAll re = findAllMono (polyToMono re) .| C.map (unsafeMatch re)
-- findAll :: MonadIO m => Poly.RE Word8 a -> ConduitT B.ByteString a m ()
-- findAll re = findAllMono (polyToMono re) .| x .| C.map (unsafeMatch re)
--  where
--     x = awaitForever $ \y -> liftIO (print y) >> yield y

replaceFirstMono :: Monad m => Mono.RE -> (L.ByteString -> L.ByteString) -> ConduitT B.ByteString B.ByteString m ()
replaceFirstMono re f = go search0
  where
    search0 = search (DFA.compile re)
    go (Found (BufferPartition dropped match leftover)) = do
        traverse yield dropped
        C.sourceLazy (f (L.fromChunks match))
        traverse yield leftover
        awaitForever yield
    go (Partial dropped done more) = do
        traverse yield dropped
        mchunk <- awaitNonNull
        case mchunk of
            Nothing -> case done of
                Left buffer -> do
                    traverse_ yield buffer
                Right (BufferPartition dropped' match leftover) -> do
                    traverse yield dropped
                    C.sourceLazy (f (L.fromChunks match))
                    traverse_ yield leftover
            Just chunk -> go (more (toNullable chunk))

replaceAllMono :: Monad m => Mono.RE -> (L.ByteString -> L.ByteString) -> ConduitT B.ByteString B.ByteString m ()
replaceAllMono re f = go search0
  where
    search0 = search (DFA.compile re)
    go (Found (BufferPartition dropped match leftover)) = do
        traverse yield dropped
        C.sourceLazy (f (L.fromChunks match))
        (traverse yield leftover >> awaitForever yield) .| go search0
    go (Partial dropped done more) = do
        traverse yield dropped
        mchunk <- awaitNonNull
        case mchunk of
            Nothing -> case done of
                Left buffer -> do
                    when (not (null buffer)) $
                        traverse_ yield buffer .| go search0
                Right (BufferPartition dropped' match leftover) -> do
                    traverse yield dropped
                    C.sourceLazy (f (L.fromChunks match))
                    when (not (null leftover)) $
                        traverse_ yield leftover .| go search0
            Just chunk -> go (more (toNullable chunk))

replaceFirst :: Monad m => Poly.RE Word8 L.ByteString -> ConduitT B.ByteString B.ByteString m ()
replaceFirst re = replaceFirstMono (polyToMono re) (unsafeMatch re)

replaceAll :: Monad m => Poly.RE Word8 L.ByteString -> ConduitT B.ByteString B.ByteString m ()
replaceAll re = replaceAllMono (polyToMono re) (unsafeMatch re)
