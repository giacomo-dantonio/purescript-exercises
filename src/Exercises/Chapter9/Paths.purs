module Exercises.Chapter9.Paths where

import Prelude
import Data.Array
import Data.Array.ST
import Data.Maybe
import Control.Monad.Eff
import Graphics.Canvas hiding (translate)

import Exercises.Chapter9.Shapes (translate)

-- Exercise 3 Section 9.4

type Point = { x :: Number, y :: Number }

showPoint :: Point -> String
showPoint p = "(" ++ show p.x ++ ", " ++ show p.y ++ ")"

renderPath :: forall eff. Context2D ->
              Array Point ->
              Eff (canvas :: Canvas | eff) Context2D
renderPath ctx points = 
  foldM (\ctx point -> lineTo ctx point.x point.y) ctx points

samples :: Number -> Number -> Array Point
samples start end = runPure $ runSTArray do
    arr <- emptySTArray
    forE start end $ \i -> do
      pushSTArray arr (f i)
      return unit
    return arr

f :: Number -> Point
f x = {x: Math.log t * radius * Math.cos t, y: Math.log t * radius * Math.sin t}
  where
    t = x / 100.0 + 1.0
    radius = 100.0


main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setStrokeStyle "#0000FF" ctx
  strokePath ctx $ renderPath ctx $ translate 300.0 300.0 <$> samples 0.0 5000.0
