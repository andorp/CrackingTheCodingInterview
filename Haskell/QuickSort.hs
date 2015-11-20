{-# LANGUAGE Rank2Types #-}
module QuickSort where

import Control.Applicative
import Control.Monad
import Control.Monad.Random hiding (fromList)
import Control.Monad.ST
import Data.Array.ST (STArray, Ix)
import STTrans as STArray

type QuickSortCtx s a = RandT StdGen (ST s) a

seed = mkStdGen 0

runQuickSortCtx :: (forall s . QuickSortCtx s a) -> a
runQuickSortCtx m = runST $ flip evalRandT seed m

testQuick :: [Int] -> [Int]
testQuick xs = runQuickSortCtx $ do
  let n = length xs
  array <- fromList xs
  quick (n - 1) array
  map snd <$> toList array

quick :: (Show e,Ord e) => Int -> STArray s Int e -> QuickSortCtx s ()
quick n array = quickSort 0 n where
  quickSort lo hi | hi < lo = return ()
  quickSort lo hi = do
    p <- partition lo hi
    quickSort lo (p - 1)
    quickSort (p + 1) hi

  partition lo hi = do
    pi <- getRandomR (lo,hi)
    swap pi hi
    pivot <- array @: hi
    vi <- newSTRef lo
    forM_ [lo .. hi - 1] $ \j -> do
      aj <- array @: j
      when (aj < pivot) $ do
        i <- readSTRef vi
        swap i j
        modifySTRef vi (+1)
    i <- readSTRef vi
    swap i hi
    return i

  swap i j | i == j = return ()
  swap i j = do
    xi <- array @: i
    xj <- array @: j
    writeSTArray array j xi
    writeSTArray array i xj
