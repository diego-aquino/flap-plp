module Models.Area where

import Models.Line (Line (Line))
import qualified Models.Line as Line
import Utils.Lists (mapWithIndex)
import Utils.Strings (removeLeadingSpaces)

newtype Area = Area
  {occupiedLines :: [Line]}
  deriving (Show)

createFromLines :: [Line] -> Area
createFromLines = Area

createFromString :: Int -> Int -> String -> Area
createFromString originX originY string =
  createFromLines (parseLines originX originY string)

merge :: Area -> Area -> Area
merge area anotherArea =
  createFromLines (occupiedLines area ++ occupiedLines anotherArea)

parseLines :: Int -> Int -> String -> [Line]
parseLines originX originY string =
  mapWithIndex
    (\stringLine index -> parseLine originX (originY + index) stringLine)
    stringLines
  where
    stringLines = lines string

parseLine :: Int -> Int -> String -> Line
parseLine baseX baseY stringLine = Line originX originY endX
  where
    originX = baseX + numberOfLeadingSpaces
    originY = baseY
    endX = originX + numberOfValidCharacters - 1
    numberOfLeadingSpaces = length stringLine - numberOfValidCharacters
    numberOfValidCharacters = length stringLineWithoutLeadingSpaces
    stringLineWithoutLeadingSpaces = removeLeadingSpaces stringLine

overlapsWith :: Area -> Area -> Bool
overlapsWith area anotherArea =
  anyLinesOverlap (occupiedLines area) (occupiedLines anotherArea)

anyLinesOverlap :: [Line] -> [Line] -> Bool
anyLinesOverlap lines otherLines =
  any (\line -> any (Line.overlapsWith line) otherLines) lines
