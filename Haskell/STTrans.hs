{-# LANGUAGE KindSignatures #-}
module STTrans where

import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans
import           Data.Array.ST (STArray(..), Ix(..))
import qualified Data.Array.ST as STArray
import           Data.STRef (STRef(..))
import qualified Data.STRef as STRef

newSTRef :: MonadTrans t => a -> t (ST s) (STRef s a)
newSTRef x = lift $ STRef.newSTRef x

readSTRef :: MonadTrans t => STRef s a -> t (ST s) a
readSTRef ref = lift $ STRef.readSTRef ref

modifySTRef :: MonadTrans t => STRef s a -> (a -> a) -> t (ST s) ()
modifySTRef ref f = lift $ STRef.modifySTRef ref f

newSTArray
  :: (MonadTrans t, Ix i) =>
     (i, i) -> e -> t (ST s) (STArray s i e)
newSTArray idx e = lift $ STArray.newArray idx e

writeSTArray
  :: (MonadTrans t, Ix i) =>
     STArray s i e -> i -> e -> t (ST s) ()
writeSTArray array idx e = lift $ STArray.writeArray array idx e

readSTArray
  :: (MonadTrans t, Ix i) =>
     STArray s i a -> i -> t (ST s) a
readSTArray array idx = lift $ STArray.readArray array idx

(@:) :: (MonadTrans t, Ix i) => STArray s i e -> i -> t (ST s) e
(@:) array idx = readSTArray array idx
{-# INLINE (@:) #-}

stGetBounds :: (MonadTrans t, Ix i) => STArray s i e -> t (ST s) (i, i)
stGetBounds array = lift $ STArray.getBounds array

fromList
  :: (MonadTrans t, Monad (t (ST s)))
  => [a] -> t (ST s) (STArray s Int a)
fromList as = do
  let n = length as
  array <- newSTArray (0,n-1) (error "array element is undefined")
  forM_ ([0..n] `zip` as) $ \(idx, e) ->
    writeSTArray array idx e
  return array

toList
  :: (MonadTrans t, Monad (t (ST s)), Enum i, Ix i)
  => STArray s i a -> t (ST s) [(i,a)]
toList array = do
  (s,e) <- stGetBounds array
  forM [s .. e] $ \i -> do
    x <- array @: i
    return (i,x)
