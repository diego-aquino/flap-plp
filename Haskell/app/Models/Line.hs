module Models.Line where

data Line = Line
  { originX :: Int,
    originY :: Int,
    endX :: Int
  }
  deriving (Show)

overlapsWith :: Line -> Line -> Bool
overlapsWith line anotherLine
  | not haveSameOriginY = False
  | originX line <= originX anotherLine = endX line >= originX anotherLine
  | otherwise = endX anotherLine >= originX line
  where
    haveSameOriginY = originY line == originY anotherLine
