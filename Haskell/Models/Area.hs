module Models.Area where

import Models.Line (Line (Line))
import Utils.Lists (mapWithIndex)
import Utils.Strings (removeLeadingSpaces)

newtype Area = Area
  {occupiedLines :: [Line]}
  deriving (Show)

createFromString :: Int -> Int -> String -> Area
createFromString originX originY string =
  Area (parseLines originX originY string)

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
