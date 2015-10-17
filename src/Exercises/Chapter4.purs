module Exercises.Chapter4 where

import Prelude

import Control.MonadPlus (guard)

import Data.Maybe

import Data.Array.Unsafe (
  head,
  tail
)

import Data.Array (
  length, 
  filter,
  null,
  (..)
)

import qualified Data.Array as A

import Data.Foldable (
  foldl,
  foldr
)

import Data.Path

import FileOperations


-- Section 4.4

-- 1

isEven :: Int -> Boolean
isEven 0 = true
isEven n | n <= 0    = isEven $ -n
         | otherwise = not $ isEven $ n-1

-- 2

countEven :: Array Int -> Int
countEven [] = 0
countEven a = 
  (if mod (head a) 2 == 0 then 1 else 0) + 
  (countEven $ tail a)


-- Section 4.7

-- 1

squares :: Array Int -> Array Int
squares = map $ \n -> n*n

-- 2

removeNegatives :: Array Int -> Array Int
removeNegatives = filter (\n -> n >= 0)

-- 3

(<$?>) :: forall a. (a -> Boolean) -> Array a -> Array a
(<$?>) = filter

removeNegatives' :: Array Int -> Array Int
removeNegatives' a = (\n -> n >= 0) <$?> a


-- Section 4.11

-- 1

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  return [i, j]

isPrime :: Int -> Boolean
isPrime = ((==) 1) <<< length <<< factors

-- 2

cartesian :: forall a. Array a -> Array a -> Array (Array a)
cartesian lhs rhs = do
  x <- lhs
  y <- rhs
  return [x, y]

-- 3

pythagorean :: Int -> Array (Array Int)
pythagorean n = do
  i <- 1 .. n
  j <- i .. n
  k <- j .. n
  guard $ i*i + j*j == k*k
  return [i, j, k]

-- 4

-- TODO


-- Section 4.15

-- 1

allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true

-- 2

-- TODO

-- 3

count :: forall a. (a -> Boolean) -> Array a -> Int
count p xs = count' 0 xs
  where
  count' n [] = n
  count' n xs = count' (if p $ head xs then n+1 else n) $ tail xs

-- 4

reverse' :: forall a. Array a -> Array a
reverse' = foldl (\xs x -> [x] ++ xs) []


-- Section 4.17

-- 1

onlyFiles :: Path -> Array Path
onlyFiles file = (if isDirectory file then [] else [file]) ++ do
  child <- ls file
  onlyFiles child


-- 2

smallestAndLargestFile :: Array Path
smallestAndLargestFile = foldl cmpFile [root, root] $ onlyFiles root
  where
  cmpFile [smallest, largest] x = 
    if size x > size largest 
    then [smallest, x]
    -- here check for Nothing, because Nothing < Just n for all n
    else if (isNothing $ size smallest) || (size x < size smallest)
    then [x, largest]
    else [smallest, largest]

-- 3

whereIs :: String -> Maybe Path
whereIs name = A.head $ findName root
  where
    matches :: Path -> Boolean
    matches path = not $ null $ filter (\child -> filename child == name) $ ls path
    findName :: Path -> Array Path
    findName path = 
       (if matches path then [path] else []) ++ do
         child <- ls path
         findName child
