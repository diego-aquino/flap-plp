{-# LANGUAGE DataKinds #-}

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
  Area.createFromString (originX bird) (originY bird) (toString bird)

toString :: Bird -> String
toString bird
  | verticalSpeed bird < 0 = "   . 7\n" ++ " // _/"
  | verticalSpeed bird > 0 = "\\\\ .\n" ++ " \\\\__\\"
  | otherwise = "== (.\n" ++ " \\___\\"
