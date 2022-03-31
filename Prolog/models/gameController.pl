:- module(controller, [initGameLoop/0]).

:- use_module(gameScreen).
:- use_module(gameState).
:- use_module(terminal).
:- use_module(localStorage).
:- use_module(bird).
:- use_module('../utils/list').

exitKeyNumber(113). % Char code for "Q"
actionKeyNumber(13). % Char code for "Enter"

gameFPS(20).
delayBetweenGameFrames(DelayInSeconds):-
  gameFPS(GameFPS),
  DelayInSeconds is 1 / GameFPS.

birdTickFPS(20).
birdJumpVerticalSpeed(-1.4).

scoreTickFPS(20).
scoreIncrement(1).

gravity(0.2).
initGameLoop:-
  terminal:hideCursor,
  terminal:startPlayerInputThread,
  bird:create(4, 5, 0, Bird),
  readHighestScore(HighestScore),
  gameState:create(Bird,PipeGroups,0,HighestScore,'paused-screen',GameState),
  run(GameState, 0).

% Stops program when the exit is typed. Stops the program correctly, but causes some problems afterwards.
haltIfExitKeyWasTyped(CharCode):-
  exitKeyNumber(CharCode),
  terminal:showCursor,
  halt,
  !.
haltIfExitKeyWasTyped(_).

processInputByScreen('playing-screen',GameState,GameStateWithInput):-
  gameState:bird(GameState,Bird),
  birdJumpVerticalSpeed(BirdJumpVerticalSpeed),
  bird:jump(Bird, BirdJumpVerticalSpeed, BirdWithInput),
  gameState:setBird(GameState,BirdWithInput,GameStateWithInput).

processInputByScreen('paused-screen',GameState,GameStateWithInput):-
  gameState:changeScreenType(GameState,'playing-screen',GameStateWithInput).

processInputByScreen('game-over-screen',GameState,GameStateWithInput):-
  gameState:changeScreenType(GameState,'playing-screen',GameStateWithInput).

processInput(GameState, CharCode, GameStateWithInput):-
  actionKeyNumber(CharCode),
  gameState:screenType(GameState,ScreenType),
  processInputByScreen(ScreenType,GameState,GameStateWithInput),!.

processInput(GameState, _, GameState).

run(GameState, ElapsedTime):-
  terminal:fetchFromThread(CharCode),
  haltIfExitKeyWasTyped(CharCode),

  % terminal:getTerminalHeight(Height),
  % terminal:getTerminalWidth(Width),

  % Change pipes

  processInput(GameState, CharCode, GameStateWithInput),
  
  tick(GameStateWithInput, ElapsedTime, TickedGameState),

  % Check collisions

  % Save high score

  terminal:moveCursorToOrigin,
  gameScreen:render(TickedGameState),

  delayBetweenGameFrames(DelayInSeconds),
  sleep(DelayInSeconds),
  NextElapsedTime is ElapsedTime + DelayInSeconds,
  run(TickedGameState, NextElapsedTime).

tick(GameState, ElapsedTime, TickedGameState):-
  gameState:screenType(GameState,'playing-screen'),

  tickScoreIfNecessary(GameState,ElapsedTime,TickedGameState1),

  tickBirdIfNecessary(TickedGameState1, ElapsedTime, TickedGameState),
  !.

tick(GameState,_,GameState).

tickScoreIfNecessary(GameState, ElapsedTime, TickedGameState):-
  gameState:score(GameState,Score),
  scoreTickFPS(ScoreTickFPS),
  NumberOfScoreFrames is floor(ElapsedTime * ScoreTickFPS),
  0 is (NumberOfScoreFrames mod 1),
  scoreIncrement(ScoreIncrement),
  gameState:incrementScore(GameState, ScoreIncrement, TickedGameState),
  !.

tickScoreIfNecessary(GameState,_,GameState).

tickBirdIfNecessary(GameState, ElapsedTime, TickedGameState):-
  gameState:bird(GameState,Bird),
  birdTickFPS(BirdTickFPS),
  NumberOfBirdFrames is floor(ElapsedTime * BirdTickFPS),
  0 is (NumberOfBirdFrames mod 1),
  gravity(Gravity),
  bird:tick(Bird, Gravity, TickedBird),
  gameState:setBird(GameState,TickedBird,TickedGameState),
  !.
tickBirdIfNecessary(GameState, _, GameState).
