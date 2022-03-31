:- module(controller, [initGameLoop/0]).

:- use_module(gameScreen).
:- use_module(terminal).
:- use_module(bird).
:- use_module(pipeGroup).
:- use_module(pipe).
:- use_module('../utils/list').

exitKeyNumber(113). % Char code for "Q"
actionKeyNumber(13). % Char code for "Enter"

gameFPS(20).
delayBetweenGameFrames(DelayInSeconds):-
  gameFPS(GameFPS),
  DelayInSeconds is 1 / GameFPS.

birdTickFPS(20).
birdJumpVerticalSpeed(-1.4).

pipeWidth(5).
pipeGroupOriginY(0).
pipeGroupHoleHeight(10).

gravity(0.2).

initGameLoop:-
  terminal:hideCursor,
  terminal:startPlayerInputThread,

  bird:create(4, 5, 0, Bird),

  pipeWidth(PipeWidth),
  pipeGroupOriginY(PipeGroupOriginY),
  pipeGroupHoleHeight(PipeGroupHoleHeight),
  pipeGroup:create(20, PipeGroupOriginY, PipeWidth, 21, 5, PipeGroupHoleHeight, PipeGroup1),
  PipeGroups = [PipeGroup1 | []],

  InitialScore = 0,
  HighestScore = 0, % temporarily hardcoded 0

  gameState:playingScreenType(PlayingScreenType),

  gameState:create(Bird, PipeGroups, InitialScore, HighestScore, PlayingScreenType, InitialGameState),
  run(InitialGameState, 0).

haltIfExitKeyWasTyped(CharCode):-
  exitKeyNumber(CharCode),
  terminal:showCursor,
  halt,
  !.
haltIfExitKeyWasTyped(_).

processInput(GameState, CharCode, GameStateWithInput):-
  actionKeyNumber(CharCode),
  birdJumpVerticalSpeed(BirdJumpVerticalSpeed),
  gameState:bird(GameState, Bird),
  bird:jump(Bird, BirdJumpVerticalSpeed, JumpedBird),
  gameState:setBird(GameState, JumpedBird, GameStateWithInput),
  !.
processInput(GameState, _, GameState).

run(GameState, ElapsedTime):-
  terminal:fetchFromThread(CharCode),
  haltIfExitKeyWasTyped(CharCode),

  % terminal:getTerminalHeight(Height), This methods are commented
  % terminal:getTerminalWidth(Width),

  % Change pipes

  processInput(GameState, CharCode, GameStateWithInput),
  tick(GameStateWithInput, ElapsedTime, TickedGameStateWithInput),

  % Tick
  % Check collisions
  % Save high score

  terminal:moveCursorToOrigin,
  gameScreen:render(TickedGameStateWithInput),

  delayBetweenGameFrames(DelayInSeconds),
  sleep(DelayInSeconds),
  NextElapsedTime is ElapsedTime + DelayInSeconds,
  run(TickedGameStateWithInput, NextElapsedTime).

tick(GameState, ElapsedTime, TickedGameState):-
  gameState:bird(GameState, Bird),
  tickBirdIfNecessary(Bird, ElapsedTime, TickedBird),
  gameState:setBird(GameState, TickedBird, TickedGameState).

tickBirdIfNecessary(Bird, ElapsedTime, TickedBird):-
  birdTickFPS(BirdTickFPS),
  NumberOfBirdFrames is floor(ElapsedTime * BirdTickFPS),
  0 is (NumberOfBirdFrames mod 1),
  gravity(Gravity),
  bird:tick(Bird, Gravity, TickedBird),
  !.
tickBirdIfNecessary(Bird, _, Bird).
