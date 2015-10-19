module Exercises.Chapter6 where

import Prelude
import Data.Picture
import Data.Array
import Data.Foldable
import Data.Monoid

import Data.Hashable


-- Section 6.3

-- 1

-- the implementation is in src/Data/Picture.purs


-- Section 6-.4

-- 1

newtype Complex = Complex
  { real :: Number, 
    imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex {
    real: real,
    imaginary: imaginary
  }) = show real ++ " + i * " ++ show imaginary

instance eqComplex :: Eq Complex where
  eq 
  (Complex {real: lr, imaginary: li}) 
  (Complex {real: rr, imaginary: ri}) = eq lr rr && eq li ri  

-- 2

data NonEmpty a = NonEmpty a (Array a)

--showArr :: foralla. (Show a) => Array a -> String
--showArr = (foldl (\x y -> (x ++ ", " ++ y)) "") <<< (map show)

instance showNonEmpty :: (Show a) => Show (NonEmpty a) where
  show (NonEmpty x xs) = show (x : xs)

instance eqNonEmpty :: (Eq a) => Eq (NonEmpty a) where
  eq (NonEmpty x xs) (NonEmpty y ys) = eq x y && eq xs ys
  
instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (append xs (y: ys))

-- 3

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty x xs) = NonEmpty (f x) (f <$> xs)

-- 4

instance foldableNonEmpty :: Foldable NonEmpty where
  foldr f a (NonEmpty x xs) = foldr f a (x:xs)
  foldl f a (NonEmpty x xs) = foldl f a (x:xs)
  foldMap f (NonEmpty x xs) = foldMap f (x:xs)

-- Section 6.7

-- 1

-- see above

-- 2

data Extended a = Finite a | Infinite

instance eqExtended :: (Eq a) => Eq (Extended a) where
  eq Infinite Infinite = true
  eq Infinite _ = false
  eq _ Infinite = false
  eq (Finite lhs) (Finite rhs) = eq lhs rhs

instance ordExtended :: (Ord a) => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare _ Infinite = LT
  compare Infinite _ = GT
  compare (Finite lhs) (Finite rhs) = compare lhs rhs

-- 3

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: (Foldable f) => Foldable (OneMore f) where
  foldl f a (OneMore x xs) = foldl f (f a x) xs
  foldr f a (OneMore x xs) = f x (foldr f a xs)
  foldMap f (OneMore x xs) = append (f x) (foldMap f xs)

-- Section 6.10

-- 1

class (Monoid m) <= Action m a where
  act :: m -> a -> a


newtype Integer = Integer Int
  
instance intSemigroup :: Semigroup Integer where
  append (Integer n) (Integer m) = Integer (n + m)


instance intMonoid :: Monoid Integer where
  mempty = Integer 0
  
instance repeatAction :: Action Integer String where
  act (Integer 0) _ = ""
  act (Integer n) s = s ++ act (Integer (n-1)) s  

-- TODO

-- 2

instance arrayAction :: (Action m a) => Action m (Array a) where
   act m = map $ act m
   
-- 3

newtype Self m = Self m

instance selfShow :: (Show m) => Show (Self m) where
  show (Self m) = show m
  
instance selfEq :: (Eq m) => Eq (Self m) where
  eq (Self x) (Self y) = eq x y

instance selfAction :: (Monoid m) => Action m (Self m) where
  act x (Self y) = Self (append x y)

-- 4

class Unsafe

--fromUnsafe :: forall a. (forall dummy. (Unsafe) => a) -> a

unsafeIndex:: forall a. (Unsafe) => Array a -> Int -> a
unsafeIndex = Data.Array.Unsafe.unsafeIndex

last' :: forall a. (Unsafe) => Array a -> a
last' arr = unsafeIndex arr $ length arr - 1

-- Section 6.11

-- 2

hasDuplicates :: forall a. (Hashable a) => Array a -> Boolean
hasDuplicates arr = 
  (not $ eq arr $ nubBy hashEqual arr) &&
  -- if duplicate hashes were found, check again for equality
  (not $ eq arr $ nub arr)

-- 3

newtype Hour = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance hashHour :: Hashable Hour where
  hash (Hour n) = hash $ mod n 12

  
