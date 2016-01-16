module Exercises.Chapter9.RandomCircle where

import Prelude
import Data.Maybe
import Data.Traversable (for)
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.Eff.DOM
import Graphics.Canvas

import Exercises.Chapter9.Random (fillAndStrokePath)

-- Section 9.8 Exercise 2

render :: forall eff. Context2D -> Eff (canvas :: Canvas, random :: RANDOM | eff) Unit
render ctx = do
  x <- random
  y <- random
  r <- random

  let path = arc ctx 
       { x     : x * 600.0
       , y     : y * 600.0
       , r     : r * 50.0
       , start : 0.0
       , end   : Math.pi * 2.0
       }
  fillAndStrokePath ctx path

  return unit


main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle "#FF0000" ctx
  setStrokeStyle "#000000" ctx

  node <- querySelector "#canvas"
  for node $ addEventListener "click" $ render ctx

  render ctx
