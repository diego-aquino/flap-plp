module Models.Bird where

import Models.Area
import qualified Models.Area as Area

data Bird = Bird
  { originX :: Int,
    originY :: Int,
    verticalSpeed :: Float
  }
  deriving (Show)

tick :: Bird -> Float -> Bird
tick bird gravity = Bird (originX bird) newOriginY newVerticalSpeed
  where
    newOriginY = originY bird + floor (verticalSpeed bird)
    newVerticalSpeed = verticalSpeed bird + gravity

setVerticalSpeed :: Bird -> Float -> Bird
setVerticalSpeed bird = Bird (originX bird) (originY bird)

getArea :: Bird -> Area
getArea bird =
  Area.createFromString (originX bird) (originY bird) (getStringRepresentation bird)

getStringRepresentation :: Bird -> String
getStringRepresentation bird = "== (.\n" ++ " \\___\\"
