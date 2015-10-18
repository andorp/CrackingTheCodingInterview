module LinkedList where

import Control.Monad
import Control.Monad.ST
import Data.STRef

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
    xs <- fromList [1..10]
    insertToTail 100 xs
    toList xs
  print $ runST $ do
    xs <- fromList [1..10]
    insertToTailIter 100 xs
    toList xs

-- * Iterative implementation

isEmpty :: List s a -> ST s Bool
isEmpty list = do
  value <- readSTRef list
  return $ case value of
    Empty    -> True
    Cons _ _ -> False

next :: (Show a) => List s a -> ST s (List s a)
next list = do
  value <- readSTRef list
  case value of
    Empty       -> error "List was empty"
    Cons _ next -> return next

insertToTailIter :: (Show a) => a -> List s a -> ST s ()
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
