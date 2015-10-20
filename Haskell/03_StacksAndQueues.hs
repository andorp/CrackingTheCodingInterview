module StacksAndQueues where

import Control.Applicative hiding (empty)
import Control.Monad.ST
import Control.Monad.State
import Data.STRef

-- * List

type List s a = STRef s (Node s a)
data Node s a
  = Empty
  | Cons a (List s a)

empty :: ST s (List s a)
empty = newSTRef Empty

node :: a -> ST s (List s a)
node a = newSTRef . Cons a =<< empty

setNext :: List s a -> List s a -> ST s ()
setNext n1 n2 = do
    Cons x next <- readSTRef n1
    writeSTRef n1 $ Cons x n2

-- * Stack

data Stack s a = Stack (STRef s [a])

stack :: ST s (Stack s a)
stack = Stack <$> newSTRef []

push :: a -> Stack s a -> ST s ()
push x (Stack s) = modifySTRef s (x:)

pop :: Stack s a -> ST s (Maybe a)
pop (Stack s) = do
  xs <- readSTRef s
  case xs of
    []     -> return Nothing
    (x:xs) -> do
      writeSTRef s xs
      return $ Just x

-- * Queue

-- Queue consists of two list, first is the front list which
-- contains all the element in front of the queue and a rear
-- list which contains all the rear element in reversed order
newtype Queue a = Queue ([a],[a])

qEmpty :: Queue a
qEmpty = Queue ([], [])

isEmpty :: Queue a -> Bool
isEmpty (Queue (f,r)) = null f

checkf :: Queue a -> Queue a
checkf (Queue ([],r)) = Queue (reverse r, [])
checkf q = q

enqueue :: a -> Queue a -> Queue a
enqueue a (Queue (f,r)) = checkf $ Queue (f,a:r)

dequeue :: Queue a -> (Queue a, Maybe a)
dequeue q@(Queue ([],_)) = (q, Nothing)
dequeue (Queue (x:f,r)) = (checkf $ Queue (f,r), Just x)

-- * Queue programs

type QProgram q a = State (Queue q) a

runQP :: QProgram q a -> (a, Queue q)
runQP p = runState p qEmpty

enQueue :: q -> QProgram q ()
enQueue = modify . enqueue

deQueue :: QProgram q (Maybe q)
deQueue = state (cross . dequeue) where
  cross (f,s) = (s,f)
