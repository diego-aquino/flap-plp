module Models.GameController where

import Control.Concurrent (threadDelay)
import Models.Bird (Bird (Bird))
import qualified Models.Bird as Bird
import qualified Models.GameScreen as GameScreen
import Models.GameState (GameState (GameState))
import qualified Models.GameState as GameState
import qualified Models.LocalStorage as LocalStorage
import qualified Models.PipeGroup as PipeGroup
import Models.Terminal (Terminal (Terminal))
import qualified Models.Terminal as Terminal
import System.Random (Random (randomR), getStdRandom)

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

pipeTickFPS :: Int
pipeTickFPS = 20

birdJumpVerticalSpeed :: Float
birdJumpVerticalSpeed = -1

timeBetweenPipeCreations :: Int
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
      terminalHeight <- Terminal.getTerminalHeight
      terminalWidth <- Terminal.getTerminalWidth
      holeOriginY <- genRandomPipeHeights 3 (terminalHeight - pipeGroupHoleHeight - 5)

      let pipeGroupHeight = terminalHeight - pipeGroupOriginY - 2
      let pipeGroupOriginX = terminalWidth + 1
      let setHoleOriginY = holeOriginY

      let currentState = setPipeGroupToState (gameState controller) elapsedTime pipeGroupOriginX setHoleOriginY pipeGroupHeight

      let stateWithInput = handlePlayerInput currentState lastCharacter
      let tickedStateWithInput =
            if GameState.screenType (gameState controller) == GameState.PLAYING
              then tick stateWithInput elapsedTime terminalWidth
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
    then
      if GameState.screenType state == GameState.PLAYING
        then GameState.jumpBird state (GameState.bird state)
        else GameState.setScreenType state GameState.PLAYING
    else state

genRandomPipeHeights :: Int -> Int -> IO Int
genRandomPipeHeights x y = getStdRandom (randomR (x, y))

setPipeGroupToState :: GameState -> Int -> Int -> Int -> Int -> GameState
setPipeGroupToState state elapsedTime originX holeOriginY pipeGroupHeight =
  if shouldCreatePipeGroup
    then newState
    else state
  where
    shouldCreatePipeGroup =
      GameState.screenType state == GameState.PLAYING
        && elapsedTime `mod` timeBetweenPipeCreations == 0
    newPipeGroupList = GameState.pipeGroups state ++ [newPipeGroup]
    newPipeGroup = PipeGroup.create originX pipeGroupOriginY pipeWidth pipeGroupHeight holeOriginY pipeGroupHoleHeight
    newState = GameState.setPipeGroups state newPipeGroupList

tick :: GameState -> Int -> Int -> GameState
tick state elapsedTime width =
  tickScoreIfNecessary
    (tickBirdIfNecessary (tickPipeGroupsIfNecessary state elapsedTime width) elapsedTime)
    elapsedTime

tickBirdIfNecessary :: GameState -> Int -> GameState
tickBirdIfNecessary state elapsedTime =
  if shouldTickBird
    then GameState.setBird state (Bird.tick bird gravity)
    else state
  where
    shouldTickBird =
      elapsedTime `mod` (microSecondsInASecond `div` birdTickFPS) == 0
    bird = GameState.bird state

tickScoreIfNecessary :: GameState -> Int -> GameState
tickScoreIfNecessary state elapsedTime =
  if shouldAddScore
    then GameState.incrementScore state scoreIncrement
    else state
  where
    shouldAddScore = elapsedTime `mod` (microSecondsInASecond `div` scoreTickFPS) == 0
    scoreIncrement = 1

tickPipeGroupsIfNecessary :: GameState -> Int -> Int -> GameState
tickPipeGroupsIfNecessary state elapsedTime width =
  if shouldTickPipe
    then GameState.setPipeGroups state (removePipeGroupIfNecessary (tickAllPipeGroups pipeGroup) width)
    else state
  where 
    shouldTickPipe = elapsedTime `mod` (microSecondsInASecond `div` pipeTickFPS) == 0
    pipeGroup = GameState.pipeGroups state

tickAllPipeGroups :: [PipeGroup.PipeGroup] -> [PipeGroup.PipeGroup]
tickAllPipeGroups pipeGroupList = [PipeGroup.tick pipeGroup | pipeGroup <- pipeGroupList]

removePipeGroupIfNecessary :: [PipeGroup.PipeGroup] -> Int -> [PipeGroup.PipeGroup]
removePipeGroupIfNecessary [] width = []
removePipeGroupIfNecessary (headPipeGroup:tailPipeGroup) width = if (PipeGroup.originX headPipeGroup) + width <= 0 then tailPipeGroup
                                          else headPipeGroup:tailPipeGroup

setGameState :: GameController -> GameState -> GameController
setGameState controller newState =
  GameController newState (terminal controller)
