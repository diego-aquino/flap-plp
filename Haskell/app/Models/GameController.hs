module Models.GameController where

import Control.Concurrent (threadDelay)
import Models.Bird (Bird (Bird))
import qualified Models.Bird as Bird
import qualified Models.GameScreen as GameScreen
import Models.GameState (GameState (GameState))
import qualified Models.GameState as GameState
import qualified Models.PipeGroup as PipeGroup
import Models.Terminal (Terminal (Terminal))
import qualified Models.Terminal as Terminal
import qualified Models.LocalStorage as LocalStorage

microSecondsInASecond :: Int
microSecondsInASecond = 1000000

gameFPS :: Int
gameFPS = 20

delayBetweenGameFrames :: Int
delayBetweenGameFrames = microSecondsInASecond `div` gameFPS

gravity :: Float
gravity = 0.2

birdTickFPS :: Int
birdTickFPS = 20

scoreTickFPS :: Int
scoreTickFPS = 20

pipeWidth :: Int
pipeWidth = 5

pipeGroupOriginY :: Int
pipeGroupOriginY = 0

pipeGroupHoleHeight :: Int
pipeGroupHoleHeight = 5

pipeGroupSpaceX :: Int
pipeGroupSpaceX = 30

birdOriginX :: Int
birdOriginX = 5

data GameController = GameController
  {gameState :: GameState, terminal :: Terminal}

createGameController :: IO GameController
createGameController = do
  terminal <- Terminal.createTerminal
  terminalHeight <- Terminal.getTerminalHeight

  let initialBirdOriginY = terminalHeight `div` 2 - 3

  let bird = Bird birdOriginX initialBirdOriginY 0
  highestScore <- LocalStorage.readHighScore
  let gameState = GameState bird [] 0 highestScore GameState.PAUSED
  let gameController = GameController gameState terminal

  return gameController

initGameLoop :: IO ()
initGameLoop = do
  gameController <- createGameController
  run gameController 0

run :: GameController -> Int -> IO ()
run controller elapsedTime = do
  let inputChar = Terminal.inputChar (terminal controller)
  lastCharacter <- Terminal.takeLastReceivedCharacter inputChar

  let shouldStop = lastCharacter == Just Terminal.interruptSignal
  if shouldStop
    then return ()
    else do
      -- terminalHeight <- Terminal.getTerminalHeight
      -- let pipeGroupHeight = terminalHeight - pipeGroupOriginY - 1
      -- [... criar pipe groups espaçados em `pipeGroupSpaceX` e alturas aleatórias]

      let stateWithInput = handlePlayerInput (gameState controller) lastCharacter
      let tickedStateWithInput = if (GameState.screenType $ gameState controller) == GameState.PLAYING
                                then tick stateWithInput elapsedTime
                                else stateWithInput

      Terminal.resetStylesAndCursor
      GameScreen.render tickedStateWithInput

      threadDelay delay

      run (setGameState controller tickedStateWithInput) (elapsedTime + delay)
  where
    delay = delayBetweenGameFrames

handlePlayerInput :: GameState -> Maybe Char -> GameState
handlePlayerInput state playerInput =
  if playerInput == Just '\n'
    then if (GameState.screenType state) == GameState.PLAYING then GameState.jumpBird state (GameState.bird state)
    else if (GameState.screenType state) == GameState.PAUSED then GameState.setScreenType state GameState.PLAYING
    else GameState.setScreenType state GameState.PLAYING
  else state

tick :: GameState -> Int -> GameState
tick state elapsedTime = scoreTickState
  where
    shouldTickBird =
      elapsedTime `mod` (microSecondsInASecond `div` birdTickFPS) == 0
    bird = GameState.bird state
    birdTickState = if shouldTickBird
    then GameState.setBird state (Bird.tick bird gravity)
    else state

    shouldAddScore = elapsedTime `mod` (microSecondsInASecond `div` scoreTickFPS) == 0
    scoreIncrement = 1
    scoreTickState = if shouldAddScore
    then GameState.incrementScore birdTickState scoreIncrement
    else state

setGameState :: GameController -> GameState -> GameController
setGameState controller newState =
  GameController newState (terminal controller)
