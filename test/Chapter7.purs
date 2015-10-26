module Test.Chapter7 where

import Prelude
import Test.Assert
import Test.QuickCheck
import Data.Maybe
import Data.Either
import Data.Validation
import Data.Traversable

import Data.AddressBook
import Exercises.Chapter7


checkOp :: (Maybe Int -> Maybe Int -> Maybe Int) -> (Int -> Int -> Int) -> Int -> Int -> Boolean
checkOp lifted op x y = (lifted (Just x) (Just y)) == Just (op x y)

checkEmpty :: (Maybe Int -> Maybe Int -> Maybe Int) -> Boolean
checkEmpty lifted = lifted (Just 1) Nothing == Nothing


chapter7 = do
  quickCheck $ checkOp (<+>) (+)
  quickCheck $ checkOp (<->) (-)
  quickCheck $ checkOp (<**>) (*)
  quickCheck $ checkOp (</>) (/)

  quickCheck $ checkEmpty (<+>)
  quickCheck $ checkEmpty (<->)
  quickCheck $ checkEmpty (<**>)
  quickCheck $ checkEmpty (</>)

  assert $ (combineMaybe $ Just (Right 1 :: Either String Int)) == Right (Just 1)
  assert $ (combineMaybe (Nothing :: Maybe (Either String Int))) == Right Nothing

  assert $ isValid $ validateState $ address "Obernstraße" "Bremen" "HB"
  assert $ not isValid $ validateState $ address "Obernstraße" "Bremen" "Bremen"

  assert $ isValid $ nonEmpty' "" " asasgasg szg     "
  assert $ not isValid $ nonEmpty' "" "    \t    "

  let tree = Branch (Branch Leaf 1 Leaf) 2 (Branch Leaf 3 Leaf)
  assert $ visitLeftToRight tree == [1, 2, 3]
  assert $ visitRightToLeft tree == [3, 2, 1]

  let maybeTree = Branch (Branch Leaf (Just 1) Leaf) (Just 2) (Branch Leaf (Just 3) Leaf)
  assert $ sequence maybeTree == Just tree
