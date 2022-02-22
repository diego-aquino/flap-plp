module Utils.Lists where

import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Sequence

createMatrix :: Int -> Int -> (Int -> Int -> a) -> [[a]]
createMatrix width height createElement =
  createMatrixRecursive width height createElement 0

createMatrixRecursive :: Int -> Int -> (Int -> Int -> a) -> Int -> [[a]]
createMatrixRecursive width height createElement currentRow
  | height <= 0 = []
  | otherwise =
    matrixLine :
    createMatrixRecursive width (height - 1) createElement (currentRow + 1)
  where
    matrixLine = createMatrixLine width currentRow createElement

createMatrixLine :: Int -> Int -> (Int -> Int -> a) -> [a]
createMatrixLine width currentRow createElement =
  mapWithIndex
    (\element currentColumn -> createElement currentRow currentColumn)
    matrixLineTemplate
  where
    matrixLineTemplate = replicate width 0

mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex function list =
  convertSequenceToList
    (Sequence.mapWithIndex (flip function) (convertListToSequence list))

convertListToSequence :: [a] -> Sequence.Seq a
convertListToSequence = Sequence.fromList

convertSequenceToList :: Sequence.Seq a -> [a]
convertSequenceToList = Foldable.toList
