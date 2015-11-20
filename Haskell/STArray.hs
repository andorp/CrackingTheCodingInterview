module STArray where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST

import Control.Monad.Random

newSTArray :: Ix i => (i, i) -> e -> ST s (STArray s i e)
newSTArray = newArray

(@:) :: Ix i => STArray s i e -> i -> ST s e
(@:) array idx = readArray array idx
{-# INLINE (@:) #-}

stGetBounds :: Ix i => STArray s i e -> ST s (i, i)
stGetBounds = getBounds

fromList :: [a] -> ST s (STArray s Int a)
fromList as = do
  let n = length as
  array <- newSTArray (0,n-1) (error "array element is undefined")
  forM_ ([0..n] `zip` as) $ \(idx, e) ->
    writeArray array idx e
  return array

toList :: (Enum i, Ix i) => STArray s i a -> ST s [(i,a)]
toList array = do
  (s,e) <- getBounds array
  forM [s .. e] $ \i -> do
    x <- array @: i
    return (i,x)
