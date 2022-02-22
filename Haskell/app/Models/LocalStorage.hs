module Models.LocalStorage where

import System.Directory
import System.IO

saveHighScore :: Int -> IO ()
saveHighScore num = do
  let stringNum = "" ++ show num
  let file = "Score.txt"
  writeFile file stringNum

readHighScore :: IO Int
readHighScore = do
  fileExists <- doesFileExist "Score.txt"
  if fileExists
    then do
      content <- openFile "Score.txt" ReadMode
      score <- hGetContents content
      let highScore = read score
      return highScore
    else return 0
