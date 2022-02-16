module Models.Line where

data Line = Line
  { originX :: Int,
    originY :: Int,
    endX :: Int
  }
  deriving (Show)
