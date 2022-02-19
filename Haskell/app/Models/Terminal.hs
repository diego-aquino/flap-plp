module Models.Terminal where

import Control.Concurrent (MVar, ThreadId, forkIO, newEmptyMVar, putMVar, tryTakeMVar)
import Data.Maybe (fromJust)
import qualified System.Console.Terminal.Size as Size
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

getTerminalWidth :: IO Int
getTerminalWidth = do
  terminalDimensions <- Size.size
  return (Size.width (fromJust terminalDimensions))

getTerminalHeight :: IO Int
getTerminalHeight = do
  terminalDimensions <- Size.size
  return (Size.height (fromJust terminalDimensions))
