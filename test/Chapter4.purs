module Test.Chapter4 where

import Prelude
import Test.QuickCheck
import Test.Assert

import Data.Array
import Data.Maybe

import Data.Path

import Exercises.Chapter4


chapter4 = do
  -- divide by 1000 to avoid recursion errors
  quickCheck \n -> isEven $ 2 * (n / 1000)
  quickCheck \n -> not $ isEven $ 2 * (n / 1000) + 1

  assert $ (countEven $ 1 .. 100) == 50
  assert $ (countEven $ 1 .. 101) == 50

  assert $ (squares $ 1 .. 10) == [1,4,9,16,25,36,49,64,81,100]

  assert $ (removeNegatives $ -10 .. 10) == 0 .. 10
  assert $ (removeNegatives' $ -10 .. 10) == 0 .. 10

  assert $ isPrime $ 2
  assert $ isPrime $ 17
  assert $ not $ isPrime $ 20
  assert $ not $ isPrime $ 99

  assert $ cartesian (1 .. 3) (5 .. 10) == 
    [[1,5],[1,6],[1,7],[1,8],[1,9],[1,10],[2,5],[2,6],[2,7],[2,8],[2,9],[2,10],[3,5],[3,6],[3,7],[3,8],[3,9],[3,10]]

  assert $ pythagorean 17 == [[3,4,5],[5,12,13],[6,8,10],[8,15,17],[9,12,15]]

  assert $ allTrue [true, true, true]
  assert $ not $ allTrue [true, true, false]

  assert $ count (\n -> n >= 0) (-10 .. 10) == 11

  quickCheck checkReverse

  assert $ show smallestAndLargestFile == "[/etc/hosts,/home/user/code/js/test.js]"

  assert $ (filename <$> whereIs "/bin/ls") == Just "/bin/"
  assert $ isNothing  $ whereIs "/bin/cat"

    where
    checkReverse :: Array Int -> Boolean
    checkReverse a = (reverse a == reverse' a)
