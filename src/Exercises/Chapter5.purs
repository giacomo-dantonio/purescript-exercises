module Exercises.Chapter5 where

import Prelude

import Data.Picture

-- Section 5.5

-- 1

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * (factorial $ n-1)

-- 2

binomial :: Int -> Int -> Int
binomial _ 0 = 1
binomial n k | n == k    = 1
             | otherwise = binomial (n-1) (k-1) + binomial (n-1) k


-- Section 5.9

type Address = { street :: String, city :: String }
type Person = { name :: String, address :: Address }

-- 1

sameCity :: Person -> Person -> Boolean
sameCity {address: { city: lhs}}  {address: { city: rhs}}
  | lhs == rhs = true
  | otherwise  = false

-- 2

--sameCity :: forall r s t u. 
--  { address :: { city :: String | s } | r } -> 
--  { address :: { city :: String | t } | u } -> Boolean

livesInCamelot :: forall r s. { address :: { city :: String | s } | r} -> Boolean
livesInCamelot {address: {city: "Camelot"}} = true
livesInCamelot _ = false

-- 3
 
fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton dflt _ = dflt


-- Section 5.13

-- 1

circleCenteredAtOrigin :: Shape
circleCenteredAtOrigin = Circle (Point {x: 0.0, y: 0.0}) 10.0
