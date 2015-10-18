module Test.Chapter5 where

import Prelude
import Test.Assert
import Exercises.Chapter5


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
