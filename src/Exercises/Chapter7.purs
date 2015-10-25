module Exercises.Chapter7 where

import Prelude
import Data.List
import Data.Maybe
import Control.Apply


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
