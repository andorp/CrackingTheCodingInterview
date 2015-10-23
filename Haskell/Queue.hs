module Queue where

newtype Queue a = Queue ([a],[a])

empty :: Queue a
empty = Queue ([], [])

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
