module Models.GameController where

import Control.Concurrent (threadDelay)
import System.Random 
import Models.Bird (Bird (Bird))
import qualified Models.Bird as Bird
import qualified Models.GameScreen as GameScreen
import Models.GameState (GameState (GameState))
import qualified Models.GameState as GameState
import qualified Models.PipeGroup as PipeGroup
import Models.Terminal (Terminal (Terminal))
import qualified Models.Terminal as Termina
import qualified Models.Terminal as Terminal

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

pipeTickFPS :: Int
pipeTickFPS = 20

birdJumpVerticalSpeed :: Float
birdJumpVerticalSpeed = -1.4

timeBetweenPipeCreations:: Int
timeBetweenPipeCreations = 2000000 

pipeWidth :: Int
pipeWidth = 5

pipeGroupOriginY :: Int
pipeGroupOriginY = 0

pipeGroupHoleHeight :: Int
pipeGroupHoleHeight = 10

birdOriginX :: Int
birdOriginX = 5

data GameController = GameController
  {gameState :: GameState, terminal :: Terminal}

createGameController :: IO GameController
createGameController = do
  terminal <- Terminal.createTerminal
  terminalHeight <- Termina.getTerminalHeight

  let initialBirdOriginY = terminalHeight `div` 2 - 3

  let bird = Bird birdOriginX initialBirdOriginY 0
  let gameState = GameState bird [] 0 0 GameState.PLAYING
  let gameController = GameController gameState terminal

  return gameController

initGameLoop :: IO ()
initGameLoop = do
  gameController <- createGameController
  run gameController 0


genRandomPipeHeights:: Int -> Int -> IO Int
genRandomPipeHeights x y = getStdRandom(randomR (x,y))

setPipeGroupToState :: GameState -> Int -> Int -> Int -> Int -> GameState
setPipeGroupToState state elapsedTime originX holeOriginY pipeGroupHeight = 
  if shouldCreatePipeGroup
    then newState
    else state
  where 
    shouldCreatePipeGroup = elapsedTime `mod` timeBetweenPipeCreations == 0
    newPipeGroupList = (GameState.pipeGroups state) ++ [newPipeGroup]
    newPipeGroup = PipeGroup.create originX pipeGroupOriginY pipeWidth pipeGroupHeight holeOriginY pipeGroupHoleHeight
    newState = GameState.setPipeGroups state newPipeGroupList

run :: GameController -> Int -> IO ()
run controller elapsedTime = do
  let inputChar = Terminal.inputChar (terminal controller)
  lastCharacter <- Terminal.takeLastReceivedCharacter inputChar

  let shouldStop = lastCharacter == Just Terminal.interruptSignal
  if shouldStop
    then return ()
    else do
      terminalHeight <- Terminal.getTerminalHeight
      terminalWidth <- Terminal.getTerminalWidth
      holeOriginY <- genRandomPipeHeights 3 (terminalHeight - pipeGroupHoleHeight - 3)

      let pipeGroupHeight = terminalHeight - pipeGroupOriginY - 1
      let pipeGroupOriginX = terminalWidth + 1
      let setHoleOriginY = holeOriginY
      
      let currentState = setPipeGroupToState (gameState controller) elapsedTime pipeGroupOriginX setHoleOriginY pipeGroupHeight
       
      let stateWithInput = handlePlayerInput currentState lastCharacter
      let tickedStateWithInput = tick stateWithInput elapsedTime

      Terminal.resetStylesAndCursor
      GameScreen.render tickedStateWithInput

      threadDelay delay

      run (setGameState controller tickedStateWithInput) (elapsedTime + delay)
  where
    delay = delayBetweenGameFrames

handlePlayerInput :: GameState -> Maybe Char -> GameState
handlePlayerInput state playerInput =
  if playerInput == Just '\n'
    then jumpBird state (GameState.bird state)
    else state


tickBirdIfNecessary :: GameState -> Int -> GameState
tickBirdIfNecessary state elapsedTime = 
  if shouldTickBird
    then GameState.setBird state (Bird.tick bird gravity)
    else state
     
  where
    shouldTickBird =
      elapsedTime `mod` (microSecondsInASecond `div` birdTickFPS) == 0 
    bird = GameState.bird state

tickAllPipeGroups :: [PipeGroup.PipeGroup] -> [PipeGroup.PipeGroup]
tickAllPipeGroups pipeGroupList = [PipeGroup.tick pipeGroup | pipeGroup <- pipeGroupList] 

tickPipeGroupsIfNecessary :: GameState -> Int -> GameState
tickPipeGroupsIfNecessary state elapsedTime = 
  if shouldTickPipe
    then if length pipeGroup == 1 
      then GameState.setPipeGroups state [(PipeGroup.tick (pipeGroup !! 0))] 
      else GameState.setPipeGroups state (tickAllPipeGroups pipeGroup)
    else state 
  where 
    shouldTickPipe = elapsedTime `mod` (microSecondsInASecond `div` pipeTickFPS) == 0
    pipeGroup = GameState.pipeGroups state 

tick :: GameState -> Int -> GameState
tick state elapsedTime = tickBirdIfNecessary (tickPipeGroupsIfNecessary state elapsedTime) elapsedTime
  
setGameState :: GameController -> GameState -> GameController
setGameState controller newState =
  GameController newState (terminal controller)

jumpBird :: GameState -> Bird -> GameState
jumpBird state bird =
  GameState.setBird state (Bird.setVerticalSpeed bird birdJumpVerticalSpeed)
