module Exercises.Chapter10 where

import Prelude
import Data.HRec
import Data.Function
import Data.Maybe


-- Section 10.14

-- 2

foreign import union :: forall a. HRec a -> HRec a -> HRec a

foreign import eqHRec :: forall a. HRec a -> HRec a -> Boolean


-- 3

foldHRec2 :: forall a r. (r -> String -> a -> r) -> r -> HRec a -> r
foldHRec2 f = runFn3 foldHRec $ mkFn3 f

showHRec2 :: forall a. (Show a) => HRec a -> String
showHRec2 rec = foldHRec2 f "empty" rec
                where
                  f acc k v = k ++ ": " ++ show v ++ ", " ++ acc 

-- 4

lookup :: forall a. String -> HRec a -> Maybe a
lookup what = foldHRec2 f Nothing
              where
                f acc k v = if k == what then Just v else acc

-- 5

foreign import mapHRec2 :: forall a b. Fn2 (String -> a -> b) (HRec a) (HRec b)
