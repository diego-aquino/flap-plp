module Models.Terminal where

--import System.Console.Terminal.Size Import para package que adiciona leitura das dimensões do terminal
import Control.Concurrent (MVar, ThreadId, forkIO, newEmptyMVar, putMVar, tryTakeMVar)
import Data.Maybe (fromJust, isJust)
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)

newtype Terminal = Terminal
  {inputChar :: MVar Char}

configureTerminalProperties :: IO ()
configureTerminalProperties = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

startPlayerInputThread :: MVar Char -> IO ThreadId
startPlayerInputThread inputChar = forkIO $ getPlayerInput inputChar

getPlayerInput :: MVar Char -> IO ()
getPlayerInput input = do
  newChar <- getChar

  putMVar input newChar

  getPlayerInput input

receivedEnter :: MVar Char -> IO Bool
receivedEnter input = do
  char <- tryTakeMVar input
  return $ char == Just '\n'

createTerminal :: IO Terminal
createTerminal = do
  configureTerminalProperties

  inputChar <- newEmptyMVar

  startPlayerInputThread inputChar

  let terminal = Terminal inputChar

  return terminal

getTerminalWidth :: Int
getTerminalWidth = 10

--getTerminalWidth = do -- Future change to getTerminalWidthFunction
--    terminalDimensions <- size
--    width $ fromJust terminalDimensions

getTerminalHeight :: Int
getTerminalHeight = 10

--getTerminalWidth = do -- Future change to getTerminalHeightFunction
--    terminalDimensions <- size
--    height $ fromJust terminalDimensions

getTerminalDimensions :: (Int, Int)
getTerminalDimensions = (getTerminalWidth, getTerminalHeight)
