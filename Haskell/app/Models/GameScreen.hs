module Models.GameScreen where

import Data.List (intercalate)
import Models.Bird (Bird (Bird))
import qualified Models.Pipe as Bird
import qualified Models.Terminal as Terminal
import Utils.Lists (createMatrix)

render :: IO ()
render = renderPlayingScreen

renderPlayingScreen :: IO ()
renderPlayingScreen = do
  terminalWidth <- Terminal.getTerminalWidth
  terminalHeight <- Terminal.getTerminalHeight

  let screenMatrix = createEmptyScreenMatrix terminalWidth (terminalHeight - 1)

  printScreenMatrix screenMatrix

createEmptyScreenMatrix :: Int -> Int -> [[String]]
createEmptyScreenMatrix width height =
  createMatrix width height (\row column -> " ")

printScreenMatrix :: [[String]] -> IO ()
printScreenMatrix screenMatrix =
  putStr (formatScreenMatrixToRender screenMatrix)

formatScreenMatrixToRender :: [[String]] -> String
formatScreenMatrixToRender screenMatrix =
  intercalate "\n" (map formatScreenMatrixLineToRender screenMatrix)

formatScreenMatrixLineToRender :: [String] -> String
formatScreenMatrixLineToRender = intercalate ""
