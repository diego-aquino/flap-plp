module Models.GameScreen where

import Data.Foldable (fold)
import Data.List (intercalate)
import Data.Maybe (isJust, isNothing)
import Models.Bird (Bird (Bird))
import qualified Models.Bird as Bird
import Models.PipeGroup (PipeGroup (PipeGroup))
import qualified Models.PipeGroup as PipeGroup
import qualified Models.Terminal as Terminal
import Utils.Lists (createMatrix, mapWithIndex)
import Utils.Maybe (justOrDefault)

type ScreenMatrixLine = [String]

type ScreenMatrix = [ScreenMatrixLine]

-- Right now, the render methods depend directly on Bird and PipeGroup. In the
-- future, they will receive a GameState object as parameter.

render :: Bird -> [PipeGroup] -> IO ()
render = renderPlayingScreen

renderPlayingScreen :: Bird -> [PipeGroup] -> IO ()
renderPlayingScreen bird pipeGroups = do
  terminalWidth <- Terminal.getTerminalWidth
  terminalHeight <- Terminal.getTerminalHeight

  let emptyScreenMatrix = createEmptyScreenMatrix terminalWidth (terminalHeight - 1)
  let populatedScreenMatrix =
        renderPipeGroupsToScreenMatrix
          pipeGroups
          (renderBirdToScreenMatrix bird emptyScreenMatrix)

  printScreenMatrix populatedScreenMatrix

createEmptyScreenMatrix :: Int -> Int -> ScreenMatrix
createEmptyScreenMatrix width height =
  createMatrix width height (\row column -> " ")

renderBirdToScreenMatrix :: Bird -> ScreenMatrix -> ScreenMatrix
renderBirdToScreenMatrix bird =
  renderObjectToScreenMatrix
    (Bird.originX bird)
    (Bird.originY bird)
    (Bird.getStringRepresentation bird)

renderPipeGroupsToScreenMatrix :: [PipeGroup] -> ScreenMatrix -> ScreenMatrix
renderPipeGroupsToScreenMatrix pipeGroups screenMatrix
  | null pipeGroups = screenMatrix
  | otherwise =
    renderPipeGroupsToScreenMatrix
      (tail pipeGroups)
      (renderPipeGroupToScreenMatrix (head pipeGroups) screenMatrix)

renderPipeGroupToScreenMatrix :: PipeGroup -> ScreenMatrix -> ScreenMatrix
renderPipeGroupToScreenMatrix pipeGroup =
  renderObjectToScreenMatrix
    (PipeGroup.originX pipeGroup)
    (PipeGroup.originY pipeGroup)
    (PipeGroup.toString pipeGroup)

renderObjectToScreenMatrix :: Int -> Int -> String -> ScreenMatrix -> ScreenMatrix
renderObjectToScreenMatrix originX originY stringRepresentation =
  mapWithIndex
    (renderObjectToScreenMatrixLine originX originY stringRepresentation)

renderObjectToScreenMatrixLine :: Int -> Int -> String -> ScreenMatrixLine -> Int -> ScreenMatrixLine
renderObjectToScreenMatrixLine originX originY stringRepresentation screenMatrixLine lineY =
  mapWithIndex
    ( \cellElement cellX ->
        renderObjectToScreenMatrixLineCell
          originX
          originY
          stringRepresentation
          cellX
          lineY
          cellElement
    )
    screenMatrixLine

renderObjectToScreenMatrixLineCell :: Int -> Int -> String -> Int -> Int -> String -> String
renderObjectToScreenMatrixLineCell originX originY stringRepresentation cellX lineY cellElement =
  characterForCell
  where
    characterForCell = justOrDefault objectCharacterForCell cellElement
    objectCharacterForCell =
      if isJust maybeObjectMatchingLine
        && cellX >= originX
        && cellX < (originX + length objectMatchingLine)
        then Just [objectMatchingLine !! (cellX - originX)]
        else Nothing
    objectMatchingLine = justOrDefault maybeObjectMatchingLine []
    maybeObjectMatchingLine =
      if lineY >= originY
        && lineY < (originY + length objectLines)
        then Just (objectLines !! (lineY - originY))
        else Nothing
    objectLines = lines stringRepresentation

printScreenMatrix :: ScreenMatrix -> IO ()
printScreenMatrix screenMatrix =
  putStr (formatScreenMatrixToRender screenMatrix)

formatScreenMatrixToRender :: ScreenMatrix -> String
formatScreenMatrixToRender screenMatrix =
  intercalate "\n" (map formatScreenMatrixLineToRender screenMatrix)

formatScreenMatrixLineToRender :: ScreenMatrixLine -> String
formatScreenMatrixLineToRender = intercalate ""
