module Test.Main where

import Prelude

import Control.Monad.Eff.Console

import Test.Chapter3 (chapter3)
import Test.Chapter4 (chapter4)


main = do
  chapter3
  chapter4
