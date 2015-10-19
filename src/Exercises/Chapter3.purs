module Exercises.Chapter3 where

import Prelude
import Data.AddressBook3
import Data.Maybe
import Data.List


-- Section 3.15

-- 2

findEntry2 :: String -> String -> AddressBook -> Maybe Entry
findEntry2 street city = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.address.street == street && entry.address.city == city


-- 3

findName :: String -> AddressBook -> Boolean
findName name = not <<< null <<< filter filterName
  where
  filterName entry = entry.firstName == name || entry.lastName == name


-- 4

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy cmpEntry
  where
  cmpEntry lhs rhs = lhs.firstName == rhs.firstName && lhs.lastName == rhs.lastName
