module HashTable.Chaining where

import Control.Monad.ST
import Data.STRef
import Data.Array.ST
import Data.Hashable
import Data.List as List

type Hash = Int

newtype HashTable s k v = HashTable (k -> Hash, STArray s Int [(k,v)])

newWithHash :: (Eq k) => Int -> (k -> Hash) -> ST s (HashTable s k v)
newWithHash size hash = do
  array <- newArray (0,size - 1) []
  let hash' k = mod (hash k) size
  return $ HashTable (hash', array)

new :: (Eq k, Hashable k) => Int -> ST s (HashTable s k v)
new size = newWithHash size hash

insert :: (Eq k) => k -> v -> HashTable s k v -> ST s ()
insert key value (HashTable (hash, array)) = do
  let place = hash key
  xs <- readArray array place
  writeArray array place ((key, value):xs)

lookup :: (Eq k) => k -> HashTable s k v -> ST s (Maybe v)
lookup key (HashTable (hash, array)) = do
  let place = hash key
  xs <- readArray array place
  return $ List.lookup key xs

remove :: (Eq k) => k -> HashTable s k v -> ST s ()
remove key (HashTable (hash, array)) = do
  let place = hash key
  xs <- readArray array place
  writeArray array place $ filter (not . (key==) . fst) xs

