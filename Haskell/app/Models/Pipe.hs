{-# LANGUAGE DataKinds #-}

module Models.Pipe where

import Data.List (intercalate)
import Models.Area
import qualified Models.Area as Area

pipeCharacter = '#'

type UP = "UP"

type DOWN = "DOWN"

data Direction = UP | DOWN deriving (Show, Eq)

data Pipe = Pipe
  { originX :: Int,
    originY :: Int,
    width :: Int,
    height :: Int,
    direction :: Direction
  }
  deriving (Show)

tick :: Pipe -> Pipe
tick pipe = setOriginX pipe (originX pipe - 1)

setOriginX :: Pipe -> Int -> Pipe
setOriginX pipe newOriginX =
  Pipe newOriginX (originY pipe) (width pipe) (height pipe) (direction pipe)

getArea :: Pipe -> Area
getArea pipe =
  Area.createFromString (originX pipe) (originY pipe) (toString pipe)

toString :: Pipe -> String
toString pipe
  | direction pipe == DOWN = toDownString pipe
  | otherwise = toUpString pipe

toDownString :: Pipe -> String
toDownString pipe = reverse (toUpString pipe)

toUpString :: Pipe -> String
toUpString pipe =
  intercalate "\n" (toStringLines pipe)

toStringLines :: Pipe -> [String]
toStringLines pipe = replicate (height pipe) line
  where
    line = replicate (width pipe) pipeCharacter
