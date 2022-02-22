module Models.GameController where

import Control.Concurrent (threadDelay)
import Models.Bird (Bird (Bird))
import qualified Models.GameScreen as GameScreen
import qualified Models.PipeGroup as PipeGroup
import qualified Models.Terminal as Terminal

defaultFPS = 20

pipeWidth = 5
pipeGroupOriginY = 0
pipeGroupHoleHeight = 5
pipeGroupSpaceX = 30

data GameController = GameController
  { -- gameState:: GameState.GameState,
    terminal :: Terminal.Terminal,
    fps :: Int
  }

createGameController :: IO GameController
createGameController = do
  -- gameState = createGameState
  terminal <- Terminal.createTerminal
  return $ GameController terminal defaultFPS

-- return $ GameController gameState terminal 30

initGameLoop :: IO ()
initGameLoop = do
  gameController <- createGameController
  run gameController 0

run :: GameController -> Int -> IO ()
run controller time = do
  -- Atualizar jogo
  -- let newState = tick $ gameState controller

  let inputChar = Terminal.inputChar (terminal controller)
  lastCharacter <- Terminal.takeLastReceivedCharacter inputChar
  let shouldStop = lastCharacter == Just Terminal.interruptSignal

  if shouldStop
    then return ()
    else do
      -- if lastCharacter == '\n' then ... else ...

      terminalHeight <- Terminal.getTerminalHeight
      let pipeGroupHeight = terminalHeight - pipeGroupOriginY - 1

      Terminal.resetStylesAndCursor

      -- GameScreen.render gameState
      GameScreen.render
        (Bird 4 5 0)
        [ PipeGroup.create 25 pipeGroupOriginY pipeWidth pipeGroupHeight 4 pipeGroupHoleHeight,
          PipeGroup.create (25 + pipeGroupSpaceX) pipeGroupOriginY pipeWidth pipeGroupHeight 6 pipeGroupHoleHeight
        ] -- static positions for now

      threadDelay delay
      --run (setControllerGameState controller newStateWithInput) (time+delay)
      run controller (time + delay)
  where
    delay = 1000000 `div` fps controller

{-Futuros métodos para quando implementar gameState

{- Processar input do jogador
handlePlayerInput:: GameController -> GameState -> IO(GameState)
handlePlayerInput controller state = do
    receivedEnter <- Terminal.receivedEnter $ Terminal.inputChar $ terminal controller
    if (receivedEnter) then return (jumpBird newState) else return newState
-}

setControllerGameState:: GameController -> GameState -> GameController
setControllerGameState controller newState = GameController newState (gameScreen controller) (terminal controller) (fps controller)

tick:: GameState -> GameState
tick state =

jumpBird:: gameState -> gameState
jumpBird state = -}
