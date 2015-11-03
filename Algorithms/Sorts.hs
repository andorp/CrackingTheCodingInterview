module Sorts where

merge :: (Ord a) => [a] -> [a]
merge []  = []
merge [a] = [a]
merge [a,b]
  | b <= a    = [b, a]
  | otherwise = [a, b]
merge as = mergeSorted (merge bs) (merge cs) where
  n = div (length as) 2
  bs = take n as
  cs = drop n as

  mergeSorted [] [] = []
  mergeSorted as [] = as
  mergeSorted [] bs = bs
  mergeSorted aas@(a:as) bbs@(b:bs)
      | a < b     = a:mergeSorted as bbs
      | otherwise = b:mergeSorted aas bs
