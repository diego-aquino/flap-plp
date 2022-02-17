module Models.PipeGroup where

import Data.List (intercalate)
import Models.Area (Area)
import qualified Models.Area as Area
import Models.Pipe (Pipe (Pipe))
import qualified Models.Pipe as Pipe

data PipeGroup = PipeGroup
  { topPipe :: Pipe,
    bottomPipe :: Pipe,
    width :: Int,
    holeHeight :: Int
  }
  deriving (Show)

create :: Int -> Int -> Int -> Int -> Int -> Int -> PipeGroup
create originX originY width height holeOriginY holeHeight =
  PipeGroup topPipe bottomPipe width holeHeight
  where
    topPipe = Pipe originX originY width topPipeHeight Pipe.DOWN
    topPipeHeight = holeOriginY - originY
    bottomPipe = Pipe originX bottomPipeOriginY width bottomPipeHeight Pipe.UP
    bottomPipeOriginY = originY + topPipeHeight + holeHeight
    bottomPipeHeight = height - topPipeHeight - holeHeight

getArea :: PipeGroup -> Area
getArea pipeGroup = Area.merge topPipeArea bottomPipeArea
  where
    topPipeArea = Pipe.getArea (topPipe pipeGroup)
    bottomPipeArea = Pipe.getArea (bottomPipe pipeGroup)

toString :: PipeGroup -> String
toString pipeGroup =
  intercalate "\n" [topPipeAsString, hole, bottomPipeAsString]
  where
    topPipeAsString = Pipe.toString (topPipe pipeGroup)
    bottomPipeAsString = Pipe.toString (bottomPipe pipeGroup)
    hole = intercalate "\n" (replicate (holeHeight pipeGroup) holeLine)
    holeLine = replicate (width pipeGroup) ' '
