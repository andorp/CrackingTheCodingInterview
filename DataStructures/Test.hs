
import Control.Monad.ST
import Data.STRef
import Data.Array.ST

newSTArray :: Ix i => (i, i) -> e -> ST s (STArray s i e)
newSTArray = newArray

test = runST $ do
  array <- newSTArray (0, 10) (0 :: Int)
  b <- readArray array 11
  return $ show b
