module Test.Chapter5 where

import Prelude
import Test.Assert
import Exercises.Chapter5

import Data.Picture
import Data.Maybe


arthur = {
  name: "King Arthur",
  address: {
    street: "King Street",
    city: "Camelot"
  }
}

lancelot = {
  name: "Sir Lancelot",
  address: {
    street: "Knight Street",
    city: "Camelot"
  }
}

brian = {
  name: "Brian of Nazareth",
  address: {
    street: "Some Street",
    city: "Nazareth"
  }
}

chapter5 = do
  assert $ factorial 3 == 6
  assert $ factorial 4 == 24
  assert $ factorial 5 == 120

  assert $ binomial 5 2 == 10
  assert $ binomial 4 2 == 6

  assert $ sameCity arthur lancelot
  assert $ not sameCity arthur brian

  assert $ fromSingleton "Knights who say" ["Ni!"] == "Ni!"
  assert $ fromSingleton "Knights who say" 
    ["Ekke", "Ekke", "Ekke", "Ekke", "Ptangya", "Zoooooooom", "Boing", "Ni!"] == "Knights who say"

  assert $ (showShape $ scaleShape (Circle (Point {x: 1.0, y: 1.0}) 5.0)) == "Circle [center: (2.0, 2.0), radius: 10.0]"
  assert $ (showShape $ scaleShape (Rectangle (Point {x: 1.0, y: 1.0}) 5.0 3.0)) == "Rectangle [center: (2.0, 2.0), width: 10.0, height: 6.0]"
  assert $ (showShape $ scaleShape (Line (Point {x: 1.0, y: 1.0}) (Point {x: -1.0, y: 1.0}))) == "Line [start: (2.0, 2.0), end: (-2.0, 2.0)]"
  assert $ (showShape $ scaleShape (Text (Point {x: 1.0, y: 1.0}) "Ni!")) == "Text [location: (2.0, 2.0), text: \"Ni!\"]"

  assert $ extractText (Circle (Point {x: 1.0, y: 1.0}) 5.0) == Nothing
  assert $ extractText (Text (Point {x: 1.0, y: 1.0}) "Ni!") == Just "Ni!"
