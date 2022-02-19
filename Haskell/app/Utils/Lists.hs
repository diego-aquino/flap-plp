module Utils.Lists where

import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Sequence

mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex function list =
  convertSequenceToList
    (Sequence.mapWithIndex (flip function) (convertListToSequence list))

convertListToSequence :: [a] -> Sequence.Seq a
convertListToSequence = Sequence.fromList

convertSequenceToList :: Sequence.Seq a -> [a]
convertSequenceToList = Foldable.toList
