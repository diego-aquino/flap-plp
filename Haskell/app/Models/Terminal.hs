module Models.Terminal where

import Control.Concurrent (MVar, ThreadId, forkIO, newEmptyMVar, putMVar, tryTakeMVar)
import Data.Maybe (fromJust)
import qualified System.Console.ANSI as ANSI
import qualified System.Console.Terminal.Size as Size
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)

interruptSignal :: Char
interruptSignal = '*'

newtype Terminal = Terminal
  {inputChar :: MVar Char}

createTerminal :: IO Terminal
createTerminal = do
  configureTerminalProperties

  inputChar <- newEmptyMVar

  installHandler sigINT (Catch $ onInterrupt inputChar) Nothing
  installHandler sigTERM (Catch $ onInterrupt inputChar) Nothing

  startPlayerInputThread inputChar

  let terminal = Terminal inputChar

  return terminal

configureTerminalProperties :: IO ()
configureTerminalProperties = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  ANSI.hideCursor

onInterrupt :: MVar Char -> IO ()
onInterrupt inputChar = do
  ANSI.showCursor
  putMVar inputChar interruptSignal

startPlayerInputThread :: MVar Char -> IO ThreadId
startPlayerInputThread inputChar = forkIO $ getPlayerInput inputChar

getPlayerInput :: MVar Char -> IO ()
getPlayerInput inputChar = do
  newChar <- getChar
  putMVar inputChar newChar

  shouldStop <- receivedInterruptSignal inputChar
  if shouldStop
    then return ()
    else do
      getPlayerInput inputChar

receivedEnter :: MVar Char -> IO Bool
receivedEnter inputChar = do
  char <- tryTakeMVar inputChar
  return $ char == Just '\n'

receivedInterruptSignal :: MVar Char -> IO Bool
receivedInterruptSignal inputChar = do
  char <- tryTakeMVar inputChar
  return $ char == Just interruptSignal

getTerminalWidth :: IO Int
getTerminalWidth = do
  terminalDimensions <- Size.size
  return (Size.width (fromJust terminalDimensions))

getTerminalHeight :: IO Int
getTerminalHeight = do
  terminalDimensions <- Size.size
  return (Size.height (fromJust terminalDimensions))

clearScreen :: IO ()
clearScreen = do
  ANSI.setSGR [ANSI.Reset]
  ANSI.clearScreen
  ANSI.setCursorPosition 0 0
