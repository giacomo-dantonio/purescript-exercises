module Test.Chapter3 where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console

import Data.List (length)
import Data.Maybe
import Data.AddressBook3

import Exercises.Chapter3

import Test.Assert


example :: Entry
example = 
  { firstName: "John"
  , lastName: "Smith"
  , address: { street: "123 Fake St."
             , city: "Faketown"
             , state: "CA"
             }
  }
     
book0 :: AddressBook
book0 = emptyBook


chapter3 = do
  let book1 = insertEntry example emptyBook

  -- Just ("Smith, John: 123 Fake St., Faketown, CA")
  assert $ 
    (showEntry <$> findEntry2 "123 Fake St." "Faketown" book1) == 
    (Just "Smith, John: 123 Fake St., Faketown, CA")

  -- Nothing
  assert $ 
    (showEntry <$> findEntry2 "123 Fake St." "XXX" book1) == 
    Nothing

  -- true
  assert $ findName "John" book1

  -- true
  assert $ findName "Smith" book1

  -- false
  assert $ not $ findName "Doe" book1

  let book2 = insertEntry example book1
  assert $ (length $ removeDuplicates book2) == 1
