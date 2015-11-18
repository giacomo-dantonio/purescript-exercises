module Exercises.Chapter9.Shapes where

-- Exercise 1 Section 9.4

import Prelude

import Data.Maybe

import Control.Monad.Eff

import Graphics.Canvas hiding (translate)

translate :: forall r. Number -> Number -> 
              { x :: Number, y :: Number | r } -> 
              { x :: Number, y :: Number | r }
translate dx dy shape = shape 
  { x = shape.x + dx
  , y = shape.y + dy
  }

main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setStrokeStyle "#0000FF" ctx

  strokePath ctx $ rect ctx $ translate (-200.0) (-200.0)
    { x: 250.0
    , y: 250.0
    , w: 100.0
    , h: 100.0
    }
  
  setStrokeStyle "#00FF00" ctx

  strokePath ctx $ arc ctx $ translate 200.0 200.0 
    { x: 300.0
    , y: 300.0
    , r: 50.0
    , start: Math.pi * 5.0 / 8.0
    , end: Math.pi * 2.0
    }
    
  setStrokeStyle "#FF0000" ctx

  strokePath ctx $ do
    moveTo ctx 300.0 260.0
    lineTo ctx 260.0 340.0
    lineTo ctx 340.0 340.0
    closePath ctx
