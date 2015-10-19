module LinkedList where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.STRef

-- Exercise 2.1
import qualified Data.Set as Set

type List s a = STRef s (Node s a)
data Node s a
  = Empty
  | Cons a (List s a)

mkEmpty :: ST s (List s a)
mkEmpty = newSTRef Empty

mkNode :: a -> ST s (List s a)
mkNode x = do
  empty <- mkEmpty
  newSTRef (Cons x empty)

fromList :: [a] -> ST s (List s a)
fromList = foldr cons mkEmpty where
  cons x mxs = do
    xs <- mxs
    newSTRef $ Cons x xs

toList :: List s a -> ST s [a]
toList list = do
  value <- readSTRef list
  case value of
    Empty -> return []
    Cons x tail -> do
      xs <- toList tail
      return (x:xs)

insertToTail :: a -> List s a -> ST s ()
insertToTail x list = do
  value <- readSTRef list
  case value of
    Empty -> do
      empty <- mkEmpty
      writeSTRef list (Cons x empty)
    Cons _ next -> insertToTail x next

testInsertToTail = do
  print $ runST $ do
    xs <- fromList ([1..10] :: [Int])
    insertToTail 100 xs
    toList xs
  print $ runST $ do
    xs <- fromList ([1..10] :: [Int])
    insertToTailIter 100 xs
    toList xs

-- * Iterative implementation

isEmpty :: List s a -> ST s Bool
isEmpty list = do
  value <- readSTRef list
  return $ case value of
    Empty    -> True
    Cons _ _ -> False

next :: List s a -> ST s (List s a)
next list = do
  value <- readSTRef list
  case value of
    Empty       -> error "List was empty"
    Cons _ next -> return next

getData :: List s a -> ST s a
getData list = do
  value <- readSTRef list
  case value of
    Empty -> error "List was empty"
    Cons x _ -> return x

insertToTailIter :: a -> List s a -> ST s ()
insertToTailIter x list = do
  end <- mkNode x
  n <- newSTRef list
  while (readSTRef n >>= next >>= fmap not . isEmpty) $ do
    writeSTRef n =<< next =<< readSTRef n
  node <- readSTRef n
  setNext node end

setNext :: List s a -> List s a -> ST s ()
setNext node tail = do
  value <- readSTRef node
  case value of
    Empty    -> error "setNext: given node was empty"
    Cons x _ -> writeSTRef node $ Cons x tail

while :: Monad m => m Bool -> m () -> m ()
while pred body = run where
  run = do
    p <- pred
    when p $ do
      body
      run


deleteNode :: (Eq a) => a -> List s a -> ST s (List s a)
deleteNode x list = do
  value <- readSTRef list
  case value of
    Empty -> return list
    Cons y tail
      | x == y -> deleteNode x tail
      | otherwise -> do
          tail' <- deleteNode x tail
          writeSTRef list $ Cons y tail'
          return list

-- Exercise 2.1
-- ============
-- Write code to remove duplicates from an unsorted linked list
-- How would you solve this problem if a temporary buffer is not allowed
-- Using the iteratively the `deleteNode`

removeDuplicates :: (Eq a, Ord a) => List s a -> ST s (List s a)
removeDuplicates = go Set.empty where
  go values list = do
    value <- readSTRef list
    case value of
      Empty -> return list
      Cons x tail
        | x `Set.member` values -> go values tail
        | otherwise -> do
            tail' <- go (Set.insert x values) tail
            writeSTRef list $ Cons x tail'
            return list

-- Exercise 2.2
-- ============
-- Implement an algorithm to find the kth to last element of a singly linked list

nthToLast :: Int -> List s a -> ST s (Maybe (Node s a))
nthToLast k list = (stepKth k list) >>= (reachEnd list) where

  stepKth 0 list = return $ Just list 
  stepKth k list = do
    value <- readSTRef list
    case value of
      Empty -> return Nothing
      Cons _ rest -> stepKth (k-1) rest

  reachEnd list Nothing       = return Nothing
  reachEnd list (Just runner) = reachEndGo list runner

  reachEndGo list runner = do
    value <- readSTRef runner
    case value of
      Empty -> fmap Just $ readSTRef list
      Cons _ rest -> do
        list' <- next list
        reachEndGo list' rest

-- Exercise 2.3
-- ============
-- Implement an algorithm to delete a node in the middle of a single linked list,
-- given only access to that node

deleteAnInnerNode :: List s a -> ST s ()
deleteAnInnerNode list = next list >>= readSTRef >>= writeSTRef list

-- Exercise 2.4
-- ============
-- Write code to partition a linked list around a value x, such that all nodes
-- less than x come before all nodes greater than or equal to x.

partition :: (Ord a) => a -> List s a -> ST s (List s a)
partition x list = do
  value <- readSTRef list
  case value of
    Empty -> mkEmpty
    Cons y rest -> do
      start <- mkNode y
      go rest start start
  where
    go list less greater = do
      value <- readSTRef list
      case value of
        Empty -> return less
        Cons y rest
          | y < x -> do
              less' <- newSTRef $ Cons y less
              list' <- next list
              go list' less' greater
          | otherwise -> do
              greater' <- mkNode y
              setNext greater greater'
              list' <- next list
              go list' less greater'

testPartition = do
  print $ runST $ do
    xs <- fromList (reverse [1..10] :: [Int])
    ys <- partition 5 xs
    toList ys

-- Exercise 2.5
-- ============
-- You have two numbers represented by a linked list, where each node contains
-- a single digit. The digits are stored in reverse order, such that the 1'st digit
-- is at the head of the list. Write a function that adds the two numbers
-- and returns the sum as a linked list

sumDigits :: List s Int -> List s Int -> ST s (List s Int)
sumDigits x y = addDigitsL x y 0
  where
    addDigitsL :: List s Int -> List s Int -> Int -> ST s (List s Int)
    addDigitsL x y c =
      join $ pure addDigits <*> readSTRef y <*> readSTRef y <*> pure c
    addDigits Empty Empty 0 = mkEmpty
    addDigits Empty Empty n = mkNode n
    addDigits (Cons x xs) (Cons y ys) n = do
      let s = x + y + n
      let c = div s 10
      let d = mod s 10
      newSTRef . Cons d =<< addDigitsL xs ys c

reverseList :: List s a -> ST s (List s a)
reverseList list = rev list =<< mkEmpty where
  rev list res = do
    value <- readSTRef list
    case value of
      Empty       -> return res
      Cons x rest -> rev rest =<< (newSTRef $ Cons x res)

sumDigitsRev x y = do
  rx <- reverseList x
  ry <- reverseList y
  reverseList =<< sumDigits rx ry
