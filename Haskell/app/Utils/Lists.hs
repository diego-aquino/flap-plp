module Utils.Lists where

import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Sequence

createMatrix :: Int -> Int -> (Int -> Int -> a) -> [[a]]
createMatrix width height createElement =
  [[createElement row column | column <- [0 .. (width -1)]] | row <- [0 .. (height -1)]]

mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex function list =
  convertSequenceToList
    (Sequence.mapWithIndex (flip function) (convertListToSequence list))

convertListToSequence :: [a] -> Sequence.Seq a
convertListToSequence = Sequence.fromList

convertSequenceToList :: Sequence.Seq a -> [a]
convertSequenceToList = Foldable.toList
