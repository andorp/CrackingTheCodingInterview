module ArraysAndStrings where

-- Exercise 1.1
import Control.Monad.ST
import Data.Char
import Data.Array.ST
import STArray

-- Exercise 1.3
import Control.Monad
import Data.List

-- Exercise 1.1
-- ============
-- Implement an algorithm to determine if a string has all unique charachters.
-- What if you cannot use additional data structures?

isUniqueChars :: String -> Bool
isUniqueChars str | length str > 256 = False
isUniqueChars str = runST $ do
  charSet <- newSTArray (0,256) False
  let run []     = return True
      run (c:cs) = do 
        let n = ord c
        val <- charSet @: n
        if val
           then return False
           else do writeArray charSet n True
                   run cs
  run str

exercise1_1 = print $ isUniqueChars ""

-- Exercise 1.3
-- ============
-- Given two string write a method to decide if on is a permutation of the other.

isPermutationNaive :: String -> String -> Bool
isPermutationNaive s t = sort s == sort t

isPermutation :: String -> String -> Bool
isPermutation s t | length s /= length t = False
isPermutation s t = runST $ do
  
  letters <- newSTArray (0,256) (0 :: Int)
  
  forM s $ \c -> do
    let n = ord c
    x <- letters @: n
    writeArray letters n (x+1)
  
  let run [] = return True
      run (c:cs) = do
        let n = ord c
        x <- letters @: n
        let x' = x - 1
        writeArray letters n x'
        if x' < 0
          then return False
          else run cs
  run t

-- Exercise 1.4
-- ============
-- Write a method to replace all spaces in a string with "%20". You may assume
-- that the string has sufficient space at the end of the string to hold the
-- additional characters, and that you are given the true length of the string.

replaceSpaces :: String -> String
replaceSpaces [] = []
replaceSpaces (' ':cs) = '%':'2':'0':replaceSpaces cs
replaceSpaces (c:cs) = c:replaceSpaces cs

-- Exercise 1.5
-- ============
-- Implement a method to perform basic string compression using the counts of
-- repeated characters. For example, the string aabcccccaaa would become a
-- a2b1c5a3. If the "compressed" string would not become smaller than the
-- original string, your method should return the original string. You
-- can assume the string has only upper and lower case letters (a-z)

compress :: String -> String
compress s =
  if (length s < length compressed)
    then s
    else compressed
  where
    compressed = concatMap encode $ groupBy (==) s
    encode cs@(c:_) = c:show (length cs)

exercise1_5 = do
  print $ compress "aabcccccaaa"
  print $ compress "aa"

-- Exercise 1.6
-- ============
-- Given an image represented by an NxN matrix, where each pixel in the image
-- is 4 bytes, write a method to rotate the image by 90 degress. Can you
-- do this in place?

rotate :: STArray s (Int,Int) Int -> ST s ()
rotate matrix = void $ do
  (_,(n,_)) <- getBounds matrix
  forM [0 .. n `div` 2 - 1] $ \layer -> do
    let first = layer
    let last = n - 1 - layer
    forM [first .. last - 1] $ \i -> do
      let offset = i - first
      -- Save top
      top <- matrix @: (first, i)
      matrix @: (last-offset, first) >>= writeArray matrix (first, i)
      matrix @: (last, last-offset)  >>= writeArray matrix (last-offset, first)
      matrix @: (i, last)            >>= writeArray matrix (last, last-offset)
      writeArray matrix (i, last) top

-- Exercise 1.7
-- ============
-- Write an algorithm such that if an element is an MxN matrix is 0,
-- its entire row and column are set to 0

setZeroes :: STArray s (Int, Int) Int -> ST s ()
setZeroes matrix = void $ do

  -- Collect which rows and columns need to be cleared
  (_,(n,m)) <- getBounds matrix
  row    <- newSTArray (0,n) False
  column <- newSTArray (0,m) False
  forM_ [(i,j) | i <- [0..n], j <- [0..m]] $ \idx@(i,j) -> do
    x <- matrix @: idx
    when (x == 0) $ do
      writeArray row    i True
      writeArray column j True

  -- Clear rows
  let nullifyRow row = forM_ [0..m] $ \j -> writeArray matrix (row,j) 0
  forM_ [0..n] $ \i -> do
    clear <- row @: i
    when clear $ nullifyRow i

  -- Clear columns
  let nullifyColumn column = forM_ [0..n] $ \i -> writeArray matrix (i,column) 0
  forM_ [0..m] $ \j -> do
    clear <- column @: j
    when clear $ nullifyColumn j


-- Exercise 1.8
-- ============
-- Assume you have a method `isSubstring` which checks if one word is a
-- substring of another. Given two strings, s1 and s2, write a code to check
-- if s2 is a rotation of s1 using only one call to `isSubstring`

isSubstring :: String -> String -> Bool
isSubstring = isInfixOf

isRotation :: String -> String -> Bool
isRotation s1 s2 = isSubstring s2 (s1 ++ s1)
