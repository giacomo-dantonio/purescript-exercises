module Exercises.Chapter8 where

import Prelude
import Data.Array (head, tail, sort, nub, foldM, length, filter)
import Data.Maybe
import Data.Foldable
import Data.List (List(..))

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Random

import Control.Monad.ST
import Data.Array.ST
import Data.Int


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

-- Section 8.17

-- 1

safeDivide :: Int -> Int -> forall eff. Eff (err :: EXCEPTION | eff) Int
safeDivide _ 0 = throwException $ error "Boom!"
safeDivide a b = return (a / b)

-- 2

data Point = Point Number Number

squareNorm :: Point -> Number
squareNorm (Point x y) = x*x + y*y


estimatePi :: Int -> forall eff. Eff (random :: RANDOM | eff) Number
estimatePi n = do
  points <- randomArray n
  let m = length $ filter (\p -> squareNorm p <= 1.0) points
  return (4.0 * toNumber m / toNumber n)

  where
  randomArray :: Int -> forall eff. Eff (random :: RANDOM | eff) (Array Point)
  randomArray n = runSTArray do
    arr <- emptySTArray
    forE 0.0 (toNumber n) $ \i -> do
      x <- randomRange (-1.0) 1.0
      y <- randomRange (-1.0) 1.0
      pushSTArray arr (Point x y)
      return unit
    return arr

-- alternative version without arrays

estimatePi' :: Int -> forall eff. Eff (random :: RANDOM | eff) Number
estimatePi' n = do
  m <- randomTests
  return (4.0 * toNumber m / toNumber n)

  where
  randomTests = runST do
    ref <- newSTRef 0
    forE 0.0 (toNumber n) $ \i -> do
      x <- randomRange (-1.0) 1.0
      y <- randomRange (-1.0) 1.0
      modifySTRef ref (if x*x + y*y <= 1.0 then add 1 else id)
      return unit
    result <- readSTRef ref
    return result

-- Section 8.19

-- 1 and 2

-- implemented in Main.purs und UI.purs
