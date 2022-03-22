:- module(controller,[initGameLoop/0]).
:- use_module(terminal).
:- use_module(bird).
:- use_module("../utils/lists").

initGameLoop:- 
    bird:testMethodBird("Ayo bird here"),nl,
    lists:testMethodLists("Sup lists is ehre as well"),nl,
    terminal:startPlayerInputThread,
    run(0,0).


checkShouldExitGame(113):- halt. %Stops program when 'q' is typed. Stops the program correctly, but causes some problems afterwards
checkShouldExitGame(_).

processInput(CurrentState,13,StateWithInput):- StateWithInput is CurrentState + 1,!.
processInput(CurrentState,_,CurrentState).

run(CurrentState,Time):- 
    terminal:fetchFromThread(Input),
    checkShouldExitGame(Input),
    %terminal:getTerminalHeight(Height), This methods are commented
    %terminal:getTerminalWidth(Width),
    
    %Change pipes
    
    processInput(CurrentState,Input,StateWithInput),

    %Tick

    %Check collisions

    %Save high score

    NextTime is Time + 1,
    sleep(1),
    Message = "Pressed Enter this amount: " + StateWithInput,
    write(Message),nl,
    ttyflush,
    run(StateWithInput,NextTime).

/*{-# LANGUAGE BlockArguments #-}

module Models.GameController where

import Control.Concurrent (threadDelay)
import Models.Area (Area (Area))
import qualified Models.Area as Area
import Models.Bird (Bird (Bird))
import qualified Models.Bird as Bird
import qualified Models.GameScreen as GameScreen
import Models.GameState (GameState (GameState))
import qualified Models.GameState as GameState
import qualified Models.LocalStorage as LocalStorage
import Models.Pipe (Pipe (Pipe))
import qualified Models.Pipe as Pipe
import Models.PipeGroup (PipeGroup)
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

createInitialGameState :: GameState.ScreenType -> IO GameState
createInitialGameState initialScreen = do
terminalHeight <- Terminal.getTerminalHeight
let initialBirdOriginY = terminalHeight `div` 2 - 3
let bird = Bird birdOriginX initialBirdOriginY 0

highestScore <- LocalStorage.readHighScore

return (GameState bird [] 0 highestScore initialScreen)

createGameController :: IO GameController
createGameController = do
terminal <- Terminal.createTerminal
gameState <- createInitialGameState GameState.PAUSED
return (GameController gameState terminal)

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

    let currentState = setPipeGroupToState (gameState controller) elapsedTime pipeGroupOriginX holeOriginY pipeGroupHeight

    stateWithInput <- handlePlayerInput currentState lastCharacter
    let tickedStateWithInput = tick stateWithInput elapsedTime

    let tickedStateAfterCheck = checkCollision tickedStateWithInput terminalHeight

    finalState <- saveHighestScoreIfNecessary tickedStateAfterCheck

    Terminal.resetStylesAndCursor
    GameScreen.render finalState

    threadDelay delay

    run (setGameState controller finalState) (elapsedTime + delay)
where
    delay = delayBetweenGameFrames

handlePlayerInput :: GameState -> Maybe Char -> IO GameState
handlePlayerInput state playerInput =
if playerInput == Just '\n'
    then
    if GameState.screenType state == GameState.PLAYING
        then return (GameState.jumpBird state (GameState.bird state))
        else
        if GameState.screenType state == GameState.GAMEOVER
            then createInitialGameState GameState.PLAYING
            else return (GameState.setScreenType state GameState.PLAYING)
    else return state

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

tick :: GameState -> Int -> GameState
tick state elapsedTime =
tickScoreIfNecessary
    (tickBirdIfNecessary (tickPipeGroupsIfNecessary state elapsedTime) elapsedTime)
    elapsedTime

tickBirdIfNecessary :: GameState -> Int -> GameState
tickBirdIfNecessary state elapsedTime =
if shouldTickBird
    then GameState.setBird state (Bird.tick bird gravity)
    else state
where
    shouldTickBird =
    GameState.screenType state == GameState.PLAYING
        && elapsedTime `mod` (microSecondsInASecond `div` birdTickFPS) == 0
    bird = GameState.bird state

tickScoreIfNecessary :: GameState -> Int -> GameState
tickScoreIfNecessary state elapsedTime =
if shouldAddScore
    then GameState.incrementScore state scoreIncrement
    else state
where
    shouldAddScore =
    GameState.screenType state == GameState.PLAYING
        && elapsedTime `mod` (microSecondsInASecond `div` scoreTickFPS) == 0
    scoreIncrement = 1

tickPipeGroupsIfNecessary :: GameState -> Int -> GameState
tickPipeGroupsIfNecessary state elapsedTime =
if shouldTickPipe
    then GameState.setPipeGroups state (removePipeGroupIfNecessary (tickAllPipeGroups pipeGroup))
    else state
where
    shouldTickPipe =
    GameState.screenType state == GameState.PLAYING
        && elapsedTime `mod` (microSecondsInASecond `div` pipeTickFPS) == 0
    pipeGroup = GameState.pipeGroups state

tickAllPipeGroups :: [PipeGroup.PipeGroup] -> [PipeGroup.PipeGroup]
tickAllPipeGroups pipeGroupList = [PipeGroup.tick pipeGroup | pipeGroup <- pipeGroupList]

removePipeGroupIfNecessary :: [PipeGroup.PipeGroup] -> [PipeGroup.PipeGroup]
removePipeGroupIfNecessary pipeGroups =
if not (null pipeGroups) && PipeGroup.originX (head pipeGroups) + pipeWidth <= 0
    then tail pipeGroups
    else pipeGroups

setGameState :: GameController -> GameState -> GameController
setGameState controller newState =
GameController newState (terminal controller)

checkCollision :: GameState -> Int -> GameState
checkCollision state terminalHeight =
if GameState.screenType state == GameState.PLAYING && isColliding
    then GameState.setScreenType state GameState.GAMEOVER
    else state
where
    isColliding =
    (Bird.getOriginY bird < 0 || Bird.getOriginY bird + Bird.getHeight bird >= terminalHeight)
        || isCollidingWithPipes state pipeGroups
    bird = GameState.bird state
    pipeGroups = GameState.pipeGroups state

saveHighestScoreIfNecessary :: GameState -> IO GameState
saveHighestScoreIfNecessary state = do
if GameState.screenType state == GameState.GAMEOVER
    && GameState.score state > GameState.highestScore state
    then do
    LocalStorage.saveHighScore $ GameState.score state
    return (GameState.setHighestScore state (GameState.score state))
    else return state

isCollidingWithPipes :: GameState -> [PipeGroup] -> Bool
isCollidingWithPipes state [] = False
isCollidingWithPipes state (headPipeGroup : tailPipeGroup) =
not (null tailPipeGroup)
    && (Area.overlapsWith (Bird.getArea bird) (PipeGroup.getArea headPipeGroup) || Area.overlapsWith (Bird.getArea bird) (PipeGroup.getArea (head tailPipeGroup)))
where
    bird = GameState.bird state
*/