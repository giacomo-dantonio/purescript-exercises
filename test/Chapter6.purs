module Test.Chapter6 where

import Prelude
import Data.Array
import Data.Foldable
import Test.Assert
import Test.QuickCheck

import Exercises.Chapter6

chapter6 = do
  assert $ show (Complex {real: 1.0, imaginary: 1.0}) == "1.0 + i * 1.0"
  assert $ (Complex {real: 1.0, imaginary: 1.0}) == (Complex {real: 1.0, imaginary: 1.0})

  assert $ append (NonEmpty 1 [2,3]) (NonEmpty 4 [5,6]) == NonEmpty 1 [2,3,4,5,6]
  
  assert $ map show (NonEmpty 1 [2, 3]) == NonEmpty "1" ["2", "3"]
  
  assert $ foldr (\x y -> show x ++ y) "" (NonEmpty 1 [2, 3]) == "123"
  assert $ foldl (\x y -> x ++ show y) "" (NonEmpty 1 [2, 3]) == "123"
  assert $ foldMap show (NonEmpty 1 [2, 3]) == "123"

  assert $ (Finite 5) <= Infinite
  assert $ not $ Infinite <= (Finite 5)
  assert $ (Infinite :: Extended Number) == Infinite

  assert $ foldl (\x y -> x ++ show y) "" (OneMore 1 [2,3]) == "123"
  assert $ foldr (\x y -> show x ++ y) "" (OneMore 1 [2,3]) == "123"
  assert $ foldMap show (OneMore 1 [2,3]) == "123"

  assert $ act (Integer 5) "Hello" == "HelloHelloHelloHelloHello"
  assert $ act (Integer 5) ["Hello", "World"] == 
    ["HelloHelloHelloHelloHello","WorldWorldWorldWorldWorld"]

  assert $ act [1,2] (Self [3,4]) == Self [1,2,3,4]

  assert $ not $ hasDuplicates (1 .. 1000)
  assert $ hasDuplicates $ (1 .. 1000) ++ [1]
  
  quickCheck (\n -> Hour n == Hour (n + 12))