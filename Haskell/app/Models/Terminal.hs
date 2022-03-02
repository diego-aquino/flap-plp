module Models.Terminal where

import Control.Concurrent (MVar, ThreadId, forkIO, newEmptyMVar, putMVar, tryTakeMVar)
import Data.Maybe (fromJust)
import qualified System.Console.ANSI as ANSI
import qualified System.Console.Terminal.Size as Size
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)
import System.Signal (installHandler, sigINT, sigTERM)
import Data.Char (chr)

interruptSignal :: Char
interruptSignal = chr 0 -- NULL character in the ASCII table

newtype Terminal = Terminal
  {inputChar :: MVar Char}

createTerminal :: IO Terminal
createTerminal = do
  configureTerminalProperties

  inputChar <- newEmptyMVar

  installHandler sigINT (\signal -> onInterrupt inputChar)
  installHandler sigTERM (\signal -> onInterrupt inputChar)

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

  let shouldStop = newChar == interruptSignal
  if shouldStop
    then return ()
    else do
      getPlayerInput inputChar

takeLastReceivedCharacter :: MVar Char -> IO (Maybe Char)
takeLastReceivedCharacter inputChar = do tryTakeMVar inputChar

getTerminalWidth :: IO Int
getTerminalWidth = do
  terminalDimensions <- Size.size
  return (Size.width (fromJust terminalDimensions))

getTerminalHeight :: IO Int
getTerminalHeight = do
  terminalDimensions <- Size.size
  return (Size.height (fromJust terminalDimensions))

resetStylesAndCursor :: IO ()
resetStylesAndCursor = do
  ANSI.setSGR [ANSI.Reset]
  ANSI.setCursorPosition 0 0
