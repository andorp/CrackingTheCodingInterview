module Trie where

import           Prelude hiding (lookup)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)

newtype Trie a b = Trie (Maybe b, Map a (Trie a b))
  deriving (Eq, Show)

empty :: Trie a b
empty = Trie (Nothing, Map.empty)

lookup :: (Eq a, Ord a) => [a] -> Trie a b -> Maybe b
lookup [] (Trie (Nothing, _)) = Nothing
lookup [] (Trie (Just x, _))  = Just x
lookup (x:xs) (Trie (_, t))   = Map.lookup x t >>= lookup xs

bind :: (Eq a, Ord a) => [a] -> b -> Trie a b -> Trie a b
bind [] x t = bind' [] t where
  bind' []     (Trie (_, m)) = Trie (Just x, m)
  bind' (k:ks) (Trie (v, m)) =
    let t = fromMaybe empty $ Map.lookup k m
    in Trie (v, Map.insert k t m)

