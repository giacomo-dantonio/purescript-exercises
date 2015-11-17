module Exercises.Chapter7 where

import Prelude
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Foldable
import Data.Validation
import Data.Traversable
import Control.Apply
import qualified Data.String.Regex as R

import Data.AddressBook
import Data.AddressBook.ValidationChapter7


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


-- Section 7.11

-- 1

data Tree a = Leaf | Branch (Tree a) a (Tree a)

instance functorTree :: Functor Tree where
  map _ Leaf = Leaf
  map f (Branch l x r) = Branch (f <$> l) (f x) (f <$> r) 

instance foldableTree :: Foldable Tree where
  foldl _ acc Leaf = acc
  foldl f acc (Branch l x r) = foldl f (f (foldl f acc l) x) r
  foldr _ acc Leaf = acc
  foldr f acc (Branch l x r) = foldr f (f x (foldr f acc r)) l
  foldMap _ Leaf = mempty
  foldMap f (Branch l x r) = foldMap f l ++ f x ++ foldMap f r


visitLeftToRight :: forall a. Tree a -> Array a
visitLeftToRight = foldl (\x y -> x ++ [y]) []

visitRightToLeft :: forall a. Tree a -> Array a
visitRightToLeft = foldr (\x y -> y ++ [x]) []


instance showTree :: (Show a) => Show (Tree a) where
  show = show <<< visitLeftToRight


instance eqTree :: (Eq a) => Eq (Tree a) where
  eq Leaf Leaf = true
  eq (Branch a b c) (Branch x y z) = (eq a x) && (eq b y) && (eq c z)
  eq _ _ = false

instance traversableTree :: Traversable Tree where
  sequence Leaf = pure Leaf
  sequence (Branch l x r) = Branch <$> sequence l <*> x <*> sequence r
  traverse f xs = sequence (f <$> xs)


-- 3

-- sequence = traverse (id :: f a -> f a) :: t (f a) -> f (t a)
-- traverse f x = sequence (f <$> x)
