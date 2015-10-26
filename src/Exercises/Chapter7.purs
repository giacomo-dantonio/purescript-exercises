module Exercises.Chapter7 where

import Prelude
import Data.List
import Data.Maybe
import Data.Validation
import Control.Apply
import qualified Data.String.Regex as R

import Data.AddressBook
import Data.AddressBook.Validation


combineList :: forall f a. (Applicative f) => List (f a) -> f (List a)
combineList Nil = pure Nil
combineList (Cons x xs) = Cons <$> x <*> combineList xs

-- Section 7.8

-- 1

liftOperator :: forall f a. (Applicative f) => (a -> a -> a) -> f a -> f a -> f a 
liftOperator op x y = lift2 op x y

(<+>) :: forall f a. (Applicative f, Semiring a) => f a -> f a -> f a
(<+>) = liftOperator (+)

(<->) :: forall f a. (Applicative f, Ring a) => f a -> f a -> f a
(<->) = liftOperator (-)

(<**>) :: forall f a. (Applicative f, Semiring a) => f a -> f a -> f a
(<**>) = liftOperator (*)

(</>) :: forall f a. (Applicative f, ModuloSemiring a) => f a -> f a -> f a
(</>) = liftOperator (/)

-- 3

combineMaybe :: forall a f. (Applicative f) => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just x) = Just <$> x


-- Section 7.10

-- 1

stateRegex :: R.Regex
stateRegex = 
  R.regex 
    "^[A-Za-z]{2}$" 
    { unicode:    false
    , sticky:     false
    , multiline:  false
    , ignoreCase: false
    , global:     false 
    }

validateState :: Address -> V Errors Address
validateState (Address o) =
  address <$> pure o.street
          <*> pure o.city
          <*> (matches "state" stateRegex o.state *> pure o.state)

-- 2

nonEmptyRegex :: R.Regex
nonEmptyRegex = 
  R.regex 
    "[^\\s]+" 
    { unicode:    false
    , sticky:     false
    , multiline:  false
    , ignoreCase: false
    , global:     false 
    }

nonEmpty' :: String -> String -> V Errors Unit
nonEmpty' field str = (matches field nonEmptyRegex str)
