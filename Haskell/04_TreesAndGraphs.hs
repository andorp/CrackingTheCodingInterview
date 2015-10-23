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
  evalStateT (bfs start) (Set.empty, Queue.empty) where

  cross (f,s)   = (s,f)
  adjacent v = graph ! v
  visit = lift . visitor

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
