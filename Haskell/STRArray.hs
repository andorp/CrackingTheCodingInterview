module STRArray where

import Control.Applicative
import Control.Monad (forM_)
import Control.Monad.ST
import Data.Array.ST

-- STArray implementation with reindexing support

data STRArray s i e = STRArray {
    reindexer :: i -> i
  , array     :: STArray s i e	
  }

new :: Ix i => (i, i) -> e -> ST s (STRArray s i e)
new idx e  = STRArray id <$> newArray idx e

reindex :: STRArray s i e -> (i -> i) -> STRArray s i e
reindex (STRArray f array) g = STRArray (f . g) array

(@:) :: Ix i => STRArray s i e -> i -> ST s e
(@:) (STRArray reindexer array) idx = readArray array (reindexer idx)
{-# INLINE (@:) #-}

fromList :: [a] -> ST s (STRArray s Int a)
fromList as = do
  let n = length as
  array <- newArray (0,n-1) (error "array element is undefined")
  forM_ ([0..n] `zip` as) $ \(idx, e) ->
    writeArray array idx e
  return (STRArray id array)
