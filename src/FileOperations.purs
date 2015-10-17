module FileOperations where

import Prelude

import Data.Path
import Data.Array
import Data.Foldable

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child
