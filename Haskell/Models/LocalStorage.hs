module Models.LocalStorage where

import System.IO
import System.Directory

saveHighScore:: Int -> IO()
saveHighScore num = do
	let stringNum = "" ++ show num
	let file = "Score.txt"
	writeFile file stringNum

readHighScore:: IO Int
readHighScore = do 
	content <- openFile "Score.txt" ReadMode
	score <- hGetContents content
	let highScore = read score
	return highScore
	