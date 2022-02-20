module Models.GameScreen where

import Data.List (intercalate)
import Data.Maybe (isJust, isNothing)
import Models.Bird (Bird (Bird))
import qualified Models.Bird as Bird
import qualified Models.Terminal as Terminal
import Utils.Lists (createMatrix, mapWithIndex)
import Utils.Maybe (justOrDefault)

type ScreenMatrixLine = [String]

type ScreenMatrix = [ScreenMatrixLine]

-- Right now, the render methods depend directly on Bird. In the future, they
-- will receive a GameState object as parameter.

render :: Bird -> IO ()
render = renderPlayingScreen

renderPlayingScreen :: Bird -> IO ()
renderPlayingScreen bird = do
  terminalWidth <- Terminal.getTerminalWidth
  terminalHeight <- Terminal.getTerminalHeight

  let screenMatrix = createEmptyScreenMatrix terminalWidth (terminalHeight - 1)
  let screenMatrixWithBird = renderBirdToScreenMatrix bird screenMatrix

  printScreenMatrix screenMatrixWithBird

createEmptyScreenMatrix :: Int -> Int -> ScreenMatrix
createEmptyScreenMatrix width height =
  createMatrix width height (\row column -> " ")

renderBirdToScreenMatrix :: Bird -> ScreenMatrix -> ScreenMatrix
renderBirdToScreenMatrix bird =
  renderObjectToScreenMatrix
    (Bird.originX bird)
    (Bird.originY bird)
    (Bird.getStringRepresentation bird)

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
