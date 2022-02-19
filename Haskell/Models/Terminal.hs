module Models.Terminal where
--import System.Console.Terminal.Size Import para package que adiciona leitura das dimensões do terminal
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, MVar, ThreadId, tryTakeMVar)
import System.IO (stdin, hSetBuffering, hSetEcho, BufferMode(NoBuffering))
import Data.Maybe (isJust,fromJust)
data Terminal = Terminal {
    inputChar:: MVar Char
}

configureTerminalProperties:: IO()
configureTerminalProperties = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

startPlayerInputThread:: MVar Char -> IO(ThreadId)
startPlayerInputThread inputChar = forkIO $ getPLayerInput inputChar

getPLayerInput :: MVar Char -> IO ()
getPLayerInput input = do
    newChar <- getChar 
    
    putMVar input newChar
    
    getPLayerInput input

receivedEnter:: MVar Char -> IO(Bool)
receivedEnter input = do
    char <-  tryTakeMVar input
    return $ (isJust char) && (fromJust char == '\n')

createTerminal:: IO(Terminal)
createTerminal = do
    configureTerminalProperties

    inputChar <- newEmptyMVar

    startPlayerInputThread inputChar
    
    let terminal = Terminal inputChar

    return terminal

getTerminalWidth:: Int
getTerminalWidth = 10 

--getTerminalWidth = do -- Future change to getTerminalWidthFunciton
--    terminalDimensions <- size
--    width $ fromJust terminalDimensions

getTerminalHeight:: Int
getTerminalHeight = 10 

--getTerminalWidth = do -- Future change to getTerminalHeightFunciton
--    terminalDimensions <- size
--    height $ fromJust terminalDimensions

getTerminalDimensions:: (Int,Int)
getTerminalDimensions = (getTerminalWidth, getTerminalHeight)