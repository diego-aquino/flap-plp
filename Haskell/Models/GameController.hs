module Models.GameController where
import qualified Models.Terminal as Terminal
import Control.Concurrent (threadDelay)

data GameController = GameController {
    -- gameState:: GameState.GameState,
    -- gameScreen:: GameScreen.GameScreen,
    terminal :: Terminal.Terminal,
    fps :: Int
}

createGameController :: IO(GameController)
createGameController = do
    --gameState = createGameState
    --gameScreen = createGameScreen
    terminal <- Terminal.createTerminal
    return $ GameController terminal 30
    -- return $ GameController gameState gameScreen terminal 30

initGameLoop :: IO()
initGameLoop = do
    gameController <- createGameController
    run gameController 0

run :: GameController -> Int -> IO()
run controller time = do
    
    -- Atualizar jogo
    -- let newState = tick $ gameState controller

    --Processar input do jogador
    receivedEnter <- Terminal.receivedEnter $ Terminal.inputChar $ terminal controller
    let newStateWithInput = if (receivedEnter) then 1 else 0
    print newStateWithInput
    
    --showGame newStateWithInput

    threadDelay delay
    run controller (time+delay)
    --run (setControllerGameState controller newStateWithInput) (time+delay)
    where
        delay = 1000000 `div` (fps controller)

{-Futuros métodos para quando implementar gameState

{- Processar input do jogador
handlePlayerInput:: GameController -> GameState -> IO(GameState)
handlePlayerInput controller state = do
    receivedEnter <- Terminal.receivedEnter $ Terminal.inputChar $ terminal controller
    if (receivedEnter) then return (jumpBird newState) else return newState
-}

{- Printar coisas na tela
showGame:: IO()
showGame = do
    printGameScreen newState-}


setControllerGameState:: GameController -> GameState -> GameController
setControllerGameState controller newState = GameController newState (gameScreen controller) (terminal controller) (fps controller)

tick:: GameState -> GameState
tick state = 

jumpBird:: gameState -> gameState
jumpBird state = -}