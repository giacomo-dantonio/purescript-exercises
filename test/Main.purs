module Test.Main where

import Prelude

import Control.Monad.Eff.Console

import Test.Chapter3 (chapter3)
import Test.Chapter4 (chapter4)
import Test.Chapter5 (chapter5)
import Test.Chapter6 (chapter6)
import Test.Chapter7 (chapter7)
import Test.Chapter8 (chapter8)


main = do
  chapter3
  chapter4
  chapter5
  chapter6
  chapter7
  chapter8
