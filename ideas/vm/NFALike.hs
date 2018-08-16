{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Regex.Poly.NFALike
    ( NFALikeRE
    , prune
    ) where

import Data.Regex.Poly.Class
import Data.Regex.Poly.Matcher

import Control.Applicative (Alternative(..))
import Control.Monad (ap)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.State.Strict (State, runState, state)
import Data.Array.ST (newArray, readArray, writeArray, STUArray)
import Data.Bifunctor (first)
import Data.Foldable (fold)
import Data.Functor.Compose (Compose(..), getCompose)
import Data.List.NonEmpty (nonEmpty)
import Data.Monoid (First(..))
import Data.Profunctor (Profunctor(..))

newtype Node s r = Node { getEdges :: [Edge s r] }

data Edge s r = Accept r | Step (s -> (Int, Node s r))

instance Functor (Edge s) where
    fmap = rmap

instance Profunctor Edge where
    dimap g f edge = case edge of
        Accept r -> Accept (f r)
        Step step -> Step ((fmap . fmap) (dimap g f) (lmap g step))

instance Functor (Node s) where
    fmap = rmap

instance Profunctor Node where
    dimap g f (Node edges) = Node (fmap (dimap g f) edges)

instance Applicative (Node s) where
    pure = Node . pure . Accept
    (<*>) = ap

instance Alternative (Node s) where
    empty = Node empty
    Node l <|> Node r = Node (l <|> r)

instance Monad (Node s) where
    return = pure
    Node edges >>= f = Node $ do
        edge <- edges
        case edge of
            Accept r -> getEdges (f r)
            Step step -> return $ Step ((fmap . fmap) (>>= f) step)

flatten :: Traversable t => Int -> t (Int, Node s r) -> (Maybe r, s -> [(Int, Node s r)])
flatten nstates nodes = runST $ do
    set <- newArray (0, nstates - 1) False :: forall s. ST s (STUArray s Int Bool)
    let flattenNode (i, Node edges) = do
            seen <- readArray set i
            if seen
                then return mempty
                else foldMap flattenEdge edges <$ writeArray set i True
        flattenEdge (Accept r) = (pure r, mempty)
        flattenEdge (Step step) = (mempty, pure . step)
    first getFirst . fold <$> traverse flattenNode nodes

squash :: Int -> (Int, Node s r) -> Matcher s r
squash nstates = go . pure
  where
    go nodes = Matcher mr $ fmap go . nonEmpty . steps
      where
        (mr, steps) = flatten nstates nodes


newtype NFALikeRE s a = P (State Int (Node s a))

instance Functor (NFALikeRE s) where
    fmap = rmap

instance Profunctor NFALikeRE where
    dimap g f (P p) = P $ dimap g f <$> p

instance Applicative (NFALikeRE s) where
    pure = P . return . pure
    P pf <*> P pa = P $ getCompose $ Compose pf <*> Compose pa

instance Alternative (NFALikeRE s) where
    empty = P $ pure empty
    P pl <|> P pr = P $ (<|>) <$> pl <*> pr
    many = manyDefault
    some = someDefault

prune :: NFALikeRE s (Maybe a) -> NFALikeRE s a
prune (P p) = P $ (>>= maybe empty pure) <$> p

instance RE (NFALikeRE s) where
    type Alphabet (NFALikeRE s) = s

    generalSymbol f = prune (f <$> dot)

    dot = P $ state $ \i -> (Node [Step ((,) i . pure)], i + 1)

    compliment a p = prune $ Nothing <$ p <|> Just a <$ many dot

    intersect (P pa) (P pb) = P $ do
        index0 <- state $ \i -> (i, (i + nstatesA * nstatesB))
        let intersectNodes (indexA, Node edgesA) (indexB, Node edgesB) = (index0 + indexA * nstatesB + indexB, Node edgesC)
              where
                edgesC = do
                    edgeA <- edgesA
                    case edgeA of
                        Accept ra -> [ Accept (ra rb) | Accept rb <- edgesB ]
                        Step stepA -> [ Step (intersectNodes <$> stepA <*> stepB) | Step stepB <- edgesB ]
        return $ snd $ intersectNodes (0, nodeA0) (0, nodeB0)
      where
        (nodeA0, nstatesA) = runState pa 1
        (nodeB0, nstatesB) = runState pb 1

    star greed extract combine base (P p) = P $ do
        node <- p
        let go acc = orWith greed (node >>= \a -> go (acc . combine a)) (pure (acc base))
        return $ extract <$> go id

    star1 greed extract combine base (P p) = P $ do
        node <- p
        let go acc = node >>= \a -> orWith greed (go (flip combine (acc a))) (pure (acc a))
        return $ extract <$> go base

    compile (P p) = squash nstates (0, node)
      where
        (node, nstates) = runState p 1
