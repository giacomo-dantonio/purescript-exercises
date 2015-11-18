module Exercises.Chapter9.Rectangle where

import Prelude

import Data.Maybe

import Control.Monad.Eff

import Graphics.Canvas hiding (translate)

import Exercises.Chapter9.Shapes (translate)

-- Exercise 2 Section 9.4

wonderfulRectangle :: Rectangle
wonderfulRectangle =
  { x: 250.0
  , y: 250.0
  , w: 100.0
  , h: 100.0
  }

main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle "#0000FF" ctx

  fillPath ctx (do
    rect ctx $ translate (-200.0) 0.0 wonderfulRectangle
    rect ctx $ translate 200.0 0.0 wonderfulRectangle
  )
  
  setFillStyle "#00FF00" ctx

  fillPath ctx (do
    arc ctx
      { x: 300.0
      , y: 300.0
      , r: 50.0
      , start: Math.pi / 5.0
      , end: -(Math.pi / 5.0 )
      }
    lineTo ctx 300.0 300.0
    closePath ctx
  )
