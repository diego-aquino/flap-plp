module Models.GameScreen where

import Data.Foldable (fold)
import Data.List (intercalate)
import Data.Maybe (isJust, isNothing)
import Models.Bird (Bird (Bird))
import qualified Models.Bird as Bird
import Models.GameState (GameState (GameState))
import qualified Models.GameState as GameState
import Models.PipeGroup (PipeGroup (PipeGroup))
import qualified Models.PipeGroup as PipeGroup
import qualified Models.Terminal as Terminal
import Utils.Lists (createMatrix, mapWithIndex)
import Utils.Maybe (justOrDefault)

type ScreenMatrixLine = [String]

type ScreenMatrix = [ScreenMatrixLine]

-- Right now, the render methods depend directly on Bird and PipeGroup. In the
-- future, they will receive a GameState object as parameter.

render :: GameState -> IO ()
render state
  | GameState.screenType state == GameState.PLAYING = renderPlayingScreen state
  | GameState.screenType state == GameState.GAMEOVER = renderGameoverScreen state
  | otherwise = renderPausedScreen state

renderGameoverScreen :: GameState -> IO ()
renderGameoverScreen gameState = do
  terminalWidth <- Terminal.getTerminalWidth
  terminalHeight <- Terminal.getTerminalHeight

  let emptyScreenMatrix = createEmptyScreenMatrix terminalWidth (terminalHeight - 1)
  let populatedScreenMatrix = renderTextToScreenMatrix ["Game Over", "Press <Enter> to play again", "High Score: " ++ (show $ GameState.highestScore gameState)] emptyScreenMatrix

  printScreenMatrix populatedScreenMatrix

renderPausedScreen :: GameState -> IO ()
renderPausedScreen gameState = do
  terminalWidth <- Terminal.getTerminalWidth
  terminalHeight <- Terminal.getTerminalHeight

  let emptyScreenMatrix = createEmptyScreenMatrix terminalWidth (terminalHeight - 1)
  let populatedScreenMatrix = renderTextToScreenMatrix ["Welcome to Flap-PLP", "Press <Enter> to flap your wings", "High Score: " ++ (show $ GameState.highestScore gameState)] emptyScreenMatrix

  printScreenMatrix populatedScreenMatrix

renderPlayingScreen :: GameState -> IO ()
renderPlayingScreen gameState = do
  terminalWidth <- Terminal.getTerminalWidth
  terminalHeight <- Terminal.getTerminalHeight

  let emptyScreenMatrix = createEmptyScreenMatrix terminalWidth (terminalHeight - 1)

  let bird = GameState.bird gameState
  let pipeGroups = GameState.pipeGroups gameState
  let score = GameState.score gameState

  let populatedScreenMatrix =
        renderScoreToScreenMatrix score $
          renderBirdToScreenMatrix bird $
            renderPipeGroupsToScreenMatrix pipeGroups emptyScreenMatrix

  printScreenMatrix populatedScreenMatrix

createEmptyScreenMatrix :: Int -> Int -> ScreenMatrix
createEmptyScreenMatrix width height =
  createMatrix width height (\row column -> " ")

renderBirdToScreenMatrix :: Bird -> ScreenMatrix -> ScreenMatrix
renderBirdToScreenMatrix bird screenMatrix =
  renderObjectToScreenMatrix
    (Bird.originX bird)
    (Bird.originY bird)
    (Bird.toString bird)
    screenMatrix

renderPipeGroupsToScreenMatrix :: [PipeGroup] -> ScreenMatrix -> ScreenMatrix
renderPipeGroupsToScreenMatrix pipeGroups screenMatrix
  | null pipeGroups = screenMatrix
  | otherwise =
    renderPipeGroupsToScreenMatrix
      (tail pipeGroups)
      (renderPipeGroupToScreenMatrix (head pipeGroups) screenMatrix)

renderPipeGroupToScreenMatrix :: PipeGroup -> ScreenMatrix -> ScreenMatrix
renderPipeGroupToScreenMatrix pipeGroup screenMatrix =
  renderObjectToScreenMatrix
    (PipeGroup.originX pipeGroup)
    (PipeGroup.originY pipeGroup)
    (PipeGroup.toString pipeGroup)
    screenMatrix

renderScoreToScreenMatrix :: Int -> ScreenMatrix -> ScreenMatrix
renderScoreToScreenMatrix score screenMtx = renderObjectToScreenMatrix scoreOriginX 0 scoreText screenMtx
  where
    scoreText = "Score: " ++ (show score)
    scoreTextLength = length scoreText
    scoreOriginX = ((getWidthScreenMatrix screenMtx) - scoreTextLength) `div` 2

renderObjectToScreenMatrix :: Int -> Int -> String -> ScreenMatrix -> ScreenMatrix
renderObjectToScreenMatrix originX originY objectString screenMatrix
  | originX >= getWidthScreenMatrix screenMatrix = screenMatrix
  | originX + objectWidth <= 0 = screenMatrix
  | originY >= getHeightScreenMatrix screenMatrix = screenMatrix
  | originY + objectHeight <= 0 = screenMatrix
  | otherwise = mapWithIndex
    ( \screenMatrixLine lineY ->
        renderObjectToScreenMatrixLine
          originX
          originY
          objectStringLines
          objectHeight
          screenMatrixLine
          lineY
    )
    screenMatrix
  where
    objectStringLines = lines objectString
    objectHeight = length objectStringLines
    objectWidth = maximum (map length objectStringLines)

renderObjectToScreenMatrixLine :: Int -> Int -> [String] -> Int -> ScreenMatrixLine -> Int -> ScreenMatrixLine
renderObjectToScreenMatrixLine originX originY objectStringLines objectHeight screenMatrixLine lineY
  | lineY < originY || lineY > originY + objectHeight = screenMatrixLine
  | otherwise =
    mapWithIndex
      ( \cellElement cellX ->
          renderObjectToScreenMatrixLineCell
            originX
            originY
            objectStringLines
            objectHeight
            cellX
            lineY
            cellElement
      )
      screenMatrixLine

renderObjectToScreenMatrixLineCell :: Int -> Int -> [String] -> Int -> Int -> Int -> String -> String
renderObjectToScreenMatrixLineCell originX originY objectStringLines objectHeight cellX lineY cellElement =
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
        && lineY < (originY + objectHeight)
        then Just (objectStringLines !! (lineY - originY))
        else Nothing

printScreenMatrix :: ScreenMatrix -> IO ()
printScreenMatrix screenMatrix =
  putStr (formatScreenMatrixToRender screenMatrix)

formatScreenMatrixToRender :: ScreenMatrix -> String
formatScreenMatrixToRender screenMatrix =
  intercalate "\n" (map formatScreenMatrixLineToRender screenMatrix)

formatScreenMatrixLineToRender :: ScreenMatrixLine -> String
formatScreenMatrixLineToRender = intercalate ""

renderTextToScreenMatrix :: [String] -> ScreenMatrix -> ScreenMatrix
renderTextToScreenMatrix (string : []) screenMtx = renderObjectToScreenMatrix stringStartX stringStartY string screenMtx
  where
    stringLength = length string
    stringStartX = (getWidthScreenMatrix screenMtx - stringLength) `div` 2
    stringStartY = (getHeightScreenMatrix screenMtx) `div` 2
renderTextToScreenMatrix (h : t) screenMtx = renderTextToScreenMatrix t (renderTextToScreenMatrixRecursive [h] ((length t) * 2) screenMtx)

renderTextToScreenMatrixRecursive :: [String] -> Int -> ScreenMatrix -> ScreenMatrix
renderTextToScreenMatrixRecursive (string : []) spacing screenMtx = renderObjectToScreenMatrix stringStartX stringStartY string screenMtx
  where
    stringLength = length string
    stringStartX = (getWidthScreenMatrix screenMtx - stringLength) `div` 2
    stringStartY = ((getHeightScreenMatrix screenMtx) `div` 2) - spacing

getHeightScreenMatrix :: ScreenMatrix -> Int
getHeightScreenMatrix screenMtx = length screenMtx

getWidthScreenMatrix :: ScreenMatrix -> Int
getWidthScreenMatrix screenMtx = length $ head screenMtx
