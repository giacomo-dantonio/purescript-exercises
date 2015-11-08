module Test.Chapter8 where

import Prelude
import Test.Assert
import Test.QuickCheck
import Data.Maybe
import Data.Array
import Data.List (toList)

import Exercises.Chapter8


filterFn :: Int -> Array Boolean
filterFn n = [mod n 17 == 1, mod n 17 == 0]

chapter8 = do
  assert $ third (1 .. 5) == (Just 3)
  assert $ third [1,2] == Nothing
  assert $ third [1] == Nothing
  assert $ third ([] :: Array Int) == Nothing

  assert $ sums [] == [0]
  assert $ sums [1, 2, 10] == [0,1,2,3,10,11,12,13]

  quickCheck (\arr -> (filterM' filterFn (toList arr)) == (toList <$> filterM filterFn arr))
