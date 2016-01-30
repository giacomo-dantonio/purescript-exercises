module Test.Chapter10 where

import Prelude
import Data.HRec
import Data.Function

import Test.Assert
import Test.QuickCheck

import Exercises.Chapter10


recA = runFn3 insert "two" 2 $ runFn3 insert "one" 1 $ empty
recB = runFn3 insert "three" 3 $ runFn3 insert "two" 2 $ empty
recC = runFn3 insert "three" 3 $ runFn3 insert "two" 2 $ runFn3 insert "one" 1 $ empty
recD = runFn3 insert "three" "three" $ runFn3 insert "two" "two" $ runFn3 insert "one" "one" $ empty

chapter8 = do
  assert $ eqHRec recA recA
  assert $ eqHRec (union recA recB) recC

  assert $ showHRec2 recC == "three: 3, two: 2, one: 1, empty"

  assert $ eqHRec (runFn2 mapHRec2 (\k v -> k) recC) recD
