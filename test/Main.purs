module Test.Main where

import Prelude

import Control.Monad.Eff.Console

import Test.Chapter3 (chapter3)
import Test.Chapter4 (chapter4)
import Test.Chapter5 (chapter5)
import Test.Chapter6 (chapter6)


main = do
  chapter3
  chapter4
  chapter5
  chapter6
