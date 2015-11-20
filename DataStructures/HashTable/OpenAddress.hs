module HashTable.OpenAddress 
{- (
    Hash
  , HashTable
  , newWithHash
  , new
  , insert
  , lookup
  , remove
  ) where
-}
where

import Control.Monad.ST
import Data.STRef
import Data.Array.ST
import Data.Hashable
import Data.List as List

type Hash = Int

newtype HashTable s k v = HashTable (k -> Hash, Hash -> Hash, STArray s Int (Maybe (k,v)))

-- Create the sequence of addresses
addresses size address start = take size $ iterate address start

newWithHash :: (Eq k) => Int -> (k -> Hash) -> (Hash -> Hash) -> ST s (HashTable s k v)
newWithHash size hash address = do
  array <- newArray (0,size - 1) Nothing
  let hash' k = mod (hash k) size
  return $ HashTable (hash', address, array)

new :: (Eq k, Hashable k) => Int -> ST s (HashTable s k v)
new size = newWithHash size hash address
  where
    address place = mod (place + 1) size

getSize :: HashTable s k v -> ST s Int
getSize (HashTable (hash, address, array)) = do
  (0, n) <- getBounds array
  return $ n + 1

newtype ItemOp s k v a = ItemOp (
        (Hash, Maybe (k, v)) ->
        (Either (ST s a) (ST s (ItemOp s k v a)))
    )

nextItemOp = Right
onCellComp = Left
resultComp = Left

-- Browse through the hash table from the place started at key
-- and operate on every touched element or return the calculated final result.
operateOnCells :: (Eq k) => ItemOp s k v a -> k -> HashTable s k v -> ST s a
operateOnCells op key table@(HashTable (hash, address, array)) = do
  size <- getSize table
  go op $ addresses size address (hash key)
  where
    go op []              = error "HashTable.OpenAddress: no free cell found in `size` steps" 
    go (ItemOp op) (i:is) = do
      val <- readArray array i
      case op (i,val) of
        Left resultComputation -> resultComputation
        Right comp -> do
          op' <- comp
          go op' is

insert :: (Eq k) => k -> v -> HashTable s k v -> ST s ()
insert key value table@(HashTable (_, _, array)) =
  operateOnCells insertionOp key table
  where
    insertionOp = ItemOp insertion
    -- Cell is occupied do nothing, move to the next cell.
    insertion (i,(Just _)) = nextItemOp $ return insertionOp
    -- Cell is free write the value.
    insertion (i,Nothing)  = onCellComp $ writeArray array i (Just (key,value))

lookup :: (Eq k) => k -> HashTable s k v -> ST s (Maybe v)
lookup key table =
  operateOnCells lookingUpOp key table
  where
    lookingUpOp = ItemOp lookingUp
    -- Reaching empty cell means no value is found
    lookingUp (i,Nothing) = onCellComp $ return Nothing
    lookingUp (i,Just (k,v))
      | k == key  = resultComp . return $ Just v
      | otherwise = nextItemOp $ return lookingUpOp

remove :: (Eq k) => k -> HashTable s k v -> ST s ()
remove key table@(HashTable (hash, address, array)) =
  operateOnCells removingOp key table
  where
    removingOp = ItemOp removing
    removing (i,Nothing) = resultComp $ return ()
    removing (i,Just (k,v))
      | k == key  = resultComp $ writeArray array i Nothing
      | otherwise = nextItemOp $ return removingOp

