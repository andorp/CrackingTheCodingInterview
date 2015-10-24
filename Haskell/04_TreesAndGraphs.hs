module TreesAndGraphs where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans
import           Control.Monad.Trans.State
import           Data.Array ((!))
import           Data.Graph (Vertex)
import qualified Data.Graph as G
import           Data.Set (Set)
import qualified Data.Set as Set
import           Queue

data Graph node = Graph { graph :: G.Graph, vertexToNode :: Vertex -> node }

type Visit m a = StateT (Set Vertex) m a

dfs_search
  :: (Monad m) => (node -> m a) -> Graph node -> Vertex -> m ()
dfs_search visitor (Graph graph node) start =
  evalStateT (dfs start) Set.empty where
  
  isVisited = gets . Set.member
  markAsVisited = modify . Set.insert
  adjacent v = graph ! v
  visit = lift . visitor

  dfs vertex = do
    visited <- isVisited vertex
    case visited of
      True  -> return ()
      False -> do
        visit $ node vertex
        markAsVisited vertex
        mapM_ dfs $ (adjacent vertex)


bfs_search
  :: (Functor m, Monad m)
  => (node -> m a) -> Graph node -> Vertex -> m ()
bfs_search visitor (Graph graph node) start =
  evalStateT (bfs start) emptyState where

  cross (f,s)   = (s,f)
  adjacent v = graph ! v
  visit = lift . visitor

  emptyState      = (Set.empty, Queue.empty)
  isVisited x     = gets   (Set.member x . fst)
  markAsVisited x = modify (first (Set.insert x))
  isEmpty         = gets   (Queue.isEmpty . snd)
  enqueue x       = modify (second (Queue.enqueue x))
  dequeue = state $ \(set,queue) ->
    let (queue',x) = Queue.dequeue queue
    in (x,(set,queue'))

  bfs vertex = do
    markAsVisited vertex
    visit $ node vertex
    enqueue vertex

    while (not <$> isEmpty) $ do
      r <- dequeue
      forM_ (adjacent vertex) $ \n -> do
        visited <- isVisited n
        unless visited $ do
          markAsVisited n
          visit $ node n
          enqueue n

-- * Helpers

while :: Monad m => m Bool -> m () -> m ()
while pred body = run where
  run = do
    p <- pred
    when p $ do
      body
      run

-- * Binary Tree

data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving (Eq, Show)

-- Exercise 4.1
-- ============
-- Implement a function to check if a binary tree is balanced. For the
-- purposes of the question, a balanced tree is defined to be a tree sucht
-- that the heights of the two subtrees of any node never differ by more
-- than one.

balanced :: Tree a -> Bool
balanced = maybe False (const True) . go where
  go Empty        = Just 0
  go (Node _ l r) = do
    bl <- go l
    br <- go r
    checkBalanced bl br
  
  checkBalanced l r
    | l - r < 2 = Just $ (max l r + 1)
    | otherwise = Nothing -}

-- http://stackoverflow.com/questions/21205213/haskell-tail-recursion-version-of-depth-of-binary-tree

-- This builds un unevaluated thunk tree in the stack
balancedCont :: Tree a -> Bool
balancedCont t = maybe False (const True) $ go t id where
  go Empty        k = k (Just 0)
  go (Node _ l r) k =
    go l $ \vl ->
    go r $ \vr ->
    k (inc <$> join $ checkBalanced <$> vl <*> vr)

  checkBalanced l r
    | l - r < 2 = Just $ (max l r)
    | otherwise = Nothing

data TreeCont a b
  = TcFunL (Tree a) (TreeCont a b)
  | TcFunR  b       (TreeCont a b)
  | TcEmpty

-- It has the advantage of storing the trees to be processed next on the heap,
-- rather than on the stack.
balancedTCont :: Tree a -> Bool
balancedTCont = maybe False (const True) . go TcEmpty where
  go k Empty        = eval k (Just 0)
  go k (Node _ l r) = go (TcFunL r k) l

  eval (TcFunL r  k) d = go (TcFunR d k) r
  eval (TcFunR dl k) d = eval k (inc <$> join $ checkBalanced <$> dl <*> d)
  eval TcEmpty       d = d

  checkBalanced l r
    | l - r < 2 = Just $ (max l r)
    | otherwise = Nothing

balancedLCont :: Tree a -> Bool
balancedLCont t = maybe False (const True) $ go (Just 0) [(Just 0, t)] where
  go balanced [] = balanced
  go balanced (t:ts) = case t of
    (b, Empty)      -> go (checkBalanced balanced b) ts
    (b, Node _ l r) -> go (checkBalanced balanced b) ((inc <$> b,l):(inc <$> b,r):ts)

  checkBalanced l r = join (cb <$> l <*> r)
    where cb l r | l - r < 2 = Just $ (max l r)
                 | otherwise = Nothing

inc = (+1)
