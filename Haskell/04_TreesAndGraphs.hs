module TreesAndGraphs where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans
import           Control.Monad.Trans.State
import           Data.Array ((!))
import           Data.Array.ST
import           Data.Graph (Vertex)
import qualified Data.Graph as G
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           ShortCut
import qualified Queue
import           STArray

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

  adjacents v = graph ! v
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
      forM_ (adjacents vertex) $ \n -> do
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
    | otherwise = Nothing

-- http://stackoverflow.com/questions/21205213/haskell-tail-recursion-version-of-depth-of-binary-tree

-- This builds un unevaluated thunk tree in the stack
balancedCont :: Tree a -> Bool
balancedCont t = maybe False (const True) $ go t id where
  go Empty        k = k (Just 0)
  go (Node _ l r) k =
    go l $ \vl ->
    go r $ \vr ->
    k (inc <$> (join $ checkBalanced <$> vl <*> vr))

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
  eval (TcFunR dl k) d = eval k (inc <$> (join $ checkBalanced <$> dl <*> d))
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

inc :: (Num a) => a -> a
inc = (+1)

-- Exercise 4.2
-- ============
-- Given a directed graph, design an algorithm to find out whetever there is
-- a route between two nodes

data SearchState = Visiting | Visited | Unvisited
  deriving (Eq, Show)

search :: (Functor m, Monad m) => Graph node -> Vertex -> Vertex -> m Bool
search g start end | start == end = return True
search (Graph graph _) start end  = runShortCutT' $ evalStateT bfs emptyState
  where
    adjacents v = graph ! v

    emptyState  = (Queue.empty, Map.empty)

    isempty = gets $ Queue.isEmpty . fst
    markNodeAs   node state = modify $ second (Map.insert node state)
    getNodeState node       = gets   $ fromMaybe Unvisited . Map.lookup node . snd
    enqueue      node       = modify $ first (Queue.enqueue node)
    dequeue = state $ \(q, sm) ->
      let (q', v) = Queue.dequeue q
      in (v, (q', sm))

    bfs = do
      enqueue start
      while (not <$> isempty) $ do
        mnode <- dequeue
        when (isJust mnode) $ do
          let u = fromJust mnode
          forM_ (adjacents u) $ \v -> do
            state <- getNodeState v
            when (state == Unvisited) $ do
              when (v == end) . lift $ final True
              markNodeAs v Visiting
              enqueue v
          markNodeAs u Visited
      return False

-- Exercise 4.3
-- ============
-- Given a sorted (increasing order) array with unique integer elements, write an
-- algorithm to create a binary search tree

-- Creates a minial BST tree from the given array
-- Supposing that the array is sorted
createMinimalBST :: STArray s Int Int -> ST s (Tree Int)
createMinimalBST array = do
  (start, end) <- getBounds array
  create start end
  where
    create start end | end < start = return Empty
    create start end = do
      let mid = (start + end) `div` 2
      Node <$> (array @: mid)
           <*> (create start (mid - 1))
           <*> (create (mid + 1) end)

testCreateMinimalBST = runST (fromList [1,2,4,6,8,10] >>= createMinimalBST)

-- Exercise 4.4
-- ============
-- Given a binary tree design an algorithm which creates a linked list of all the nodes
-- at each depth.

leveledLists :: Tree a -> Map Int [Tree a]
leveledLists = go Map.empty 0 where
  go m  level Empty = m
  go m0 level n@(Node x left right) =
    let m1 = insert level n m0
        m2 = go m1 (level + 1) left
        m3 = go m2 (level + 1) right
    in m3
    where
      insert k x m = case Map.lookup k m of
        Nothing -> Map.insert k [x]    m
        Just xs -> Map.insert k (x:xs) m

-- Exercise 4.5
-- ============
-- Implement a function to check if a binary tree is a binary search tree.

checkBST :: (Ord a) => Tree a -> Bool
checkBST = go Nothing Nothing where
  go _ _ Empty = True
  go min max (Node x left right) =
     and [ fromMaybe True $ fmap (x >)  min
         , fromMaybe True $ fmap (x <=) max
         , go min (Just x) left
         , go (Just x) max right
         ]

-- Exercise 4.6
-- ============
-- Write an algorithm to find the 'next' node of a given node in a binary
-- search tree.

inorderSuc :: Tree a -> [Tree a]
inorderSuc = reverse . go [] where
  go xs  Empty = Empty:xs
  go xs0 n@(Node _ left right) =
    let xs1 = go (xs0)   left
        xs2 = n:xs1
        xs3 = go xs2 right
    in xs3
