module Exercises.Chapter10 where

import Prelude

import Data.HRec
import Data.Function
import Data.Maybe
import Data.Either
import Data.Foreign
import Data.Foreign.Class

import Control.Monad.Eff
import Control.Monad.Eff.Storage


-- Section 10.14

-- 1

-- > import Prelude
-- > import Data.HRec
-- > import Data.Function
-- > runFn3 insert "first name" "Leto" empty
-- runFn3 insert "first name" "Leto" $ empty
-- > runFn3 insert "last name" "Atreides" $ runFn3 insert "first name" "Leto" empty
-- runFn3 insert "last name" "Atreides" $ runFn3 insert "first name" "Leto" $ empty


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


-- Section 10.16

-- 1

foreign import data CONFIRM :: !

foreign import confirm :: String -> forall eff. Eff (confirm :: CONFIRM | eff) Unit


-- 2

foreign import removeItem :: String -> forall eff. Eff (storage :: STORAGE | eff) Unit


-- Section 10.19

-- 1

-- > readJSON "[[1, 2, 3], [4, 5], [6]]" :: F (Array (Array Int))
-- Right ([[1,2,3],[4,5],[6]])

-- > map runNull <$> (readJSON "[null, 10]" :: F (Array (Null Number)))
-- Right ([Nothing,Just (10.0)])

-- > traverse runNull <$> (readJSON "[null, 10]" :: F (Array (Null Number)))
-- Right (Nothing)

-- > map (traverse runNull) <$> (readJSON "[[null, 10], [20, 39]]" :: F (Array (Array (Null Number))))
-- Right ([Nothing,Just ([20.0,39.0])])

-- > traverse (traverse runNull) <$> (readJSON "[[null, 10], [20, 39]]" :: F (Array (Array (Null Number))))
-- Right (Nothing)

-- > traverse (traverse runNull) <$> (readJSON "[[5, 10], [20, 39]]" :: F (Array (Array (Null Number))))
-- Right (Just ([[5.0,10.0],[20.0,39.0]]))

-- 2

-- See src/Main10.purs


-- 4

newtype Tagged a b = Tagged (Either a b)

--readTagged :: forall a b. (IsForeign a, IsForeign b) => Foreign -> F (Tagged a b)
--readTagged json = do
--  tag <- readProp "tag" json
--  value <- readProp "value" json
--
--  case tag of
--    "Left" -> return Tagged (Left value)
--    "Right" -> return Tagged (Right value)
  

--instance taggedIsForeign :: (IsForeign a, IsForeign b) => IsForeign (Tagged a b) where
--  read = readTagged
