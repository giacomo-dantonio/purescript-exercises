module Exercises.Chapter8 where

import Prelude
import Data.Array (head, tail, sort, nub, foldM)
import Data.Maybe
import Data.Foldable
import Data.List (List(..))

-- Section 8.7

-- 1

third :: forall a. Array a -> Maybe a
-- third arr = tail arr >>= tail >>= head
third arr = do
  slice1 <- tail arr
  slice2 <- tail slice1
  head slice2


-- 2

--foldM :: forall m a b. (Monad m) => (a -> b -> m a) -> a -> List b -> m a
sums :: Array Int -> Array Int
--sums = sort <<< nub <<< map sum <<< foldM (\xs x -> [xs, xs ++ [x]]) []  
sums = sort <<< nub <<< foldM (\x y -> [x, y, x+y]) 0


-- 5

filterM' :: forall m a. (Monad m) => (a -> m Boolean) -> List a -> m (List a)
filterM' _ Nil = return Nil
filterM' f (Cons b bs) = do
  match <- f b
  bs' <- filterM' f bs
  return (if match then Cons b bs' else bs')


-- 6

-- f :: a -> b -> c

-- (<$>) :: (a -> b) -> (m a -> m b)
-- (<*>) :: m (a -> b) -> m a -> m b

-- f <$> return a :: m (b -> c) 
-- f <$> return a = do
--   x <- return a
--   return f x = (left identity)
-- return f a

-- lift2 f (return a) (return b) = 
--  f <$> return a <*> return b = (see above)
--  return (f a) <*> return b = do
--    g <- return (f a)
--    y <- return b
--    return (g y) = (left identity, twice)
--  return (f a b)
