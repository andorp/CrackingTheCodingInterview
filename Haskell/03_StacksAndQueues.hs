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

-- Exercise 3.7
-- ============

-- * Animal shelter

type Name = String

type TimeStamp = Int

newtype Dog = Dog (Name,TimeStamp)
newtype Cat = Cat (Name,TimeStamp)

class TimeStamped t where
  timeStamp :: t -> TimeStamp

instance TimeStamped Dog where
  timeStamp (Dog (_,t)) = t 

instance TimeStamped Cat where
  timeStamp (Cat (_,t)) = t 

type Animal = Either Dog Cat

class IsAnimal a where
  animal :: a -> Animal

instance IsAnimal Dog where
  animal = Left 

instance IsAnimal Cat where
  animal = Right

data Shelter = Shelter {
    dogs :: [Dog]
  , cats :: [Cat]
  }

enqueueAnimal :: (IsAnimal a) => a -> Shelter -> Shelter
enqueueAnimal = enqueue . animal where
  enqueue (Left dog)  (Shelter dogs cats) = Shelter (dog:dogs) cats
  enqueue (Right cat) (Shelter dogs cats) = Shelter dogs (cat:cats)

deQueueCat :: Shelter -> (Maybe Cat, Shelter)
deQueueCat (Shelter dogs (c:cats)) = (Just c, Shelter dogs cats)
deQueueCat (Shelter dogs [])       = (Nothing, Shelter dogs [])

deQueueDog :: Shelter -> (Maybe Dog, Shelter)
deQueueDog (Shelter (d:dogs) cats) = (Just d, Shelter dogs cats)
deQueueDog (Shelter [] cats)       = (Nothing,Shelter []   cats)

deQueueAnimal :: Shelter -> (Maybe Animal, Shelter)
deQueueAnimal s@(Shelter [] [])     = (Nothing, s)
deQueueAnimal (Shelter (d:dogs) []) = (Just $ animal d, Shelter dogs [])
deQueueAnimal (Shelter [] (c:cats)) = (Just $ animal c, Shelter [] cats)
deQueueAnimal (Shelter ds@(d:dogs) cs@(c:cats))
  | timeStamp d < timeStamp c = (Just $ animal d, Shelter dogs cs)
  | otherwise                 = (Just $ animal c, Shelter ds cats)
