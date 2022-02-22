module Models.Bird where

import Models.Area
import qualified Models.Area as Area

data Bird = Bird
  { originX :: Int,
    originY :: Int,
    verticalSpeed :: Int
  }
  deriving (Show)

tick :: Bird -> Bird
tick bird = setOriginY bird (originY bird + verticalSpeed bird)

setOriginY :: Bird -> Int -> Bird
setOriginY bird newOriginY =
  Bird (originX bird) newOriginY (verticalSpeed bird)

getArea :: Bird -> Area
getArea bird =
  Area.createFromString (originX bird) (originY bird) (getStringRepresentation bird)

getStringRepresentation :: Bird -> String
getStringRepresentation bird = "== (.\n" ++ " \\___\\"
