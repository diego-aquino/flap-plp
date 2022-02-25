{-# LANGUAGE DataKinds #-}

module Models.GameState where

import Models.Bird (Bird, setVerticalSpeed)
import Models.PipeGroup (PipeGroup)

type PAUSED = "PAUSED"

type PLAYING = "PLAYING"

type GAMEOVER = "GAMEOVER"

data ScreenType = PAUSED | PLAYING | GAMEOVER deriving (Show, Eq)

birdJumpVerticalSpeed :: Float
birdJumpVerticalSpeed = -2

data GameState = GameState
  { bird :: Bird,
    pipeGroups :: [PipeGroup],
    score :: Int,
    highestScore :: Int,
    screenType :: ScreenType
  }

setBird :: GameState -> Bird -> GameState
setBird gameState newBird =
  GameState newBird (pipeGroups gameState) (score gameState) (highestScore gameState) (screenType gameState)

setPipeGroups :: GameState -> [PipeGroup] -> GameState
setPipeGroups gameState newPipeGroups =
  GameState (bird gameState) newPipeGroups (score gameState) (highestScore gameState) (screenType gameState)

incrementScore :: GameState -> Int -> GameState
incrementScore gameState increment =
  GameState (bird gameState) (pipeGroups gameState) (increment + score gameState) (highestScore gameState) (screenType gameState)

setHighestScore :: GameState -> Int -> GameState
setHighestScore gameState newHighestScore =
  GameState (bird gameState) (pipeGroups gameState) (score gameState) newHighestScore (screenType gameState)

setScreenType :: GameState -> ScreenType -> GameState
setScreenType gameState = GameState (bird gameState) (pipeGroups gameState) (score gameState) (highestScore gameState)

jumpBird :: GameState -> Bird -> GameState
jumpBird state bird =
  setBird state (setVerticalSpeed bird birdJumpVerticalSpeed)
