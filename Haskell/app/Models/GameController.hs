module Models.GameController where

import Control.Concurrent (threadDelay)
import Models.Bird (Bird (Bird))
import qualified Models.GameScreen as GameScreen
import qualified Models.Terminal as Terminal

data GameController = GameController {
  -- gameState:: GameState.GameState,
  terminal :: Terminal.Terminal,
  fps :: Int
}

createGameController :: IO GameController
createGameController = do
  -- gameState = createGameState
  terminal <- Terminal.createTerminal
  return $ GameController terminal 5
  -- return $ GameController gameState terminal 30

initGameLoop :: IO ()
initGameLoop = do
  gameController <- createGameController
  run gameController 0

run :: GameController -> Int -> IO ()
run controller time = do
  -- Atualizar jogo
  -- let newState = tick $ gameState controller

  --Processar input do jogador
  -- receivedEnter <- Terminal.receivedEnter $ Terminal.inputChar $ terminal controller
  -- let newStateWithInput = if (receivedEnter) then 1 else 0
  -- print newStateWithInput

  -- GameScreen.render gameState
  Terminal.clearScreen
  GameScreen.render (Bird 4 5 0) -- static position for now

  threadDelay delay
  run controller (time + delay)
  --run (setControllerGameState controller newStateWithInput) (time+delay)

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
