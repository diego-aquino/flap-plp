:- module(controller, [initGameLoop/0]).

:- use_module(gameScreen).
:- use_module(gameState).
:- use_module(terminal).
:- use_module(localStorage).
:- use_module(bird).
:- use_module(pipeGroup).
:- use_module('../utils/list').

exitKeyNumber(113). % Char code for "Q"
actionKeyNumber(13). % Char code for "Enter"

gameFPS(20).
delayBetweenGameFrames(DelayInSeconds):-
  gameFPS(GameFPS),
  DelayInSeconds is 1 / GameFPS.

birdTickFPS(20).
birdJumpVerticalSpeed(-1.4).

gravity(0.2).

pipeTickFPS(20).
pipeWidth(5).
pipeGroupOriginY(0).
pipeGroupHoleHeight(10).
timeBetweenPipeCreations(2).

scoreTickFPS(20).
scoreIncrement(1).

initGameLoop:-
  terminal:hideCursor,
  terminal:startPlayerInputThread,

  bird:create(4, 5, 0, Bird),
  InitialScore = 0,
  localStorage:readHighestScore(HighestScore),
  gameState:pausedScreenType(InitialScreenType),
  gameState:create(Bird, [], InitialScore, HighestScore, InitialScreenType, InitialGameState),
  run(InitialGameState, 10).

haltIfExitKeyWasTyped(CharCode):-
  exitKeyNumber(CharCode),
  terminal:showCursor,
  halt,
  !.
haltIfExitKeyWasTyped(_).

processInputByScreen(ScreenType, GameState, GameStateWithInput):-
  gameState:playingScreenType(ScreenType),
  gameState:bird(GameState,Bird),
  birdJumpVerticalSpeed(BirdJumpVerticalSpeed),
  bird:jump(Bird, BirdJumpVerticalSpeed, BirdWithInput),
  gameState:setBird(GameState,BirdWithInput,GameStateWithInput).

processInputByScreen(ScreenType, GameState, GameStateWithInput):-
  gameState:pausedScreenType(ScreenType),
  gameState:playingScreenType(PlayingScreenType),
  gameState:setScreenType(GameState, PlayingScreenType, GameStateWithInput).

processInputByScreen(ScreenType, GameState, GameStateWithInput):-
  gameState:gameOverScreenType(ScreenType),
  gameState:playingScreenType(PlayingScreenType),
  gameState:setScreenType(GameState, PlayingScreenType, GameStateWithInput).

processInput(GameState, CharCode, GameStateWithInput):-
  actionKeyNumber(CharCode),
  gameState:screenType(GameState, ScreenType),
  processInputByScreen(ScreenType,GameState, GameStateWithInput),
  !.
processInput(GameState, _, GameState).

run(GameState, ElapsedTime):-
  terminal:fetchFromThread(CharCode),
  haltIfExitKeyWasTyped(CharCode),

  terminal:getHeight(Height),
  terminal:getWidth(Width),

  pipeGroupHoleHeight(HoleHeight),
  MinHoleOriginY = 3,
  MaxHoleOriginY is Height - HoleHeight - 5,
  random(MinHoleOriginY, MaxHoleOriginY, HoleOriginY),

  pipeGroupOriginY(PipeGroupOriginY),
  PipeGroupHeight is Height - PipeGroupOriginY - 2,
  PipeGroupOriginX is Width + 1,

  processInput(GameState, CharCode, GameStateWithInput),
  createPipeGroupIfNecessary(GameStateWithInput, ElapsedTime, PipeGroupOriginX, HoleOriginY, PipeGroupHeight, GameStateWithNewPipeGroup),
  tick(GameStateWithNewPipeGroup, ElapsedTime, TickedGameState),

  % Check collisions
  % Save high score

  terminal:moveCursorToOrigin,
  gameScreen:render(TickedGameState),

  delayBetweenGameFrames(DelayInSeconds),
  sleep(DelayInSeconds),
  NextElapsedTime is ElapsedTime + DelayInSeconds,
  run(TickedGameState, NextElapsedTime).

shouldCreatePipeGroup(ScreenType, ElapsedTime):-
  gameState:playingScreenType(ScreenType),
  timeBetweenPipeCreations(TimeBetweenPipeCreations),
  BaseElapsedTime is floor(ElapsedTime * 100),
  BaseTimeBetweenPipeCreations is floor(TimeBetweenPipeCreations * 100),
  0 is BaseElapsedTime mod BaseTimeBetweenPipeCreations.

createNewPipeGroup(OriginX, HoleOriginY, PipeGroupHeight, PipeGroup):-
  pipeGroupHoleHeight(HoleHeight),
  pipeGroupOriginY(OriginY),
  pipeWidth(Width),
  pipeGroup:create(OriginX, OriginY, Width, PipeGroupHeight, HoleOriginY, HoleHeight, PipeGroup).

createPipeGroupIfNecessary(GameState, ElapsedTime, OriginX, HoleOriginY, PipeGroupHeight, NewGameState):-
  gameState:screenType(GameState, ScreenType),
  gameState:pipeGroups(GameState, PipeGroups),

  shouldCreatePipeGroup(ScreenType, ElapsedTime),

  createNewPipeGroup(OriginX, HoleOriginY, PipeGroupHeight, NewPipeGroup),
  append(PipeGroups, [NewPipeGroup], NewPipeGroupList),
  gameState:setPipeGroups(GameState, NewPipeGroupList, NewGameState),
  !.
createPipeGroupIfNecessary(GameState, _, _, _, _, GameState).

tick(GameState, ElapsedTime, TickedGameState):-
  tickBirdIfNecessary(GameState, ElapsedTime, GameStateWithTickedBird),
  tickPipeGroupsIfNecessary(GameStateWithTickedBird, ElapsedTime, GameStateWithTickedBirdAndPipeGroups),
  tickScoreIfNecessary(GameStateWithTickedBirdAndPipeGroups, ElapsedTime, TickedGameState).

shouldTickBird(ScreenType, ElapsedTime):-
  gameState:playingScreenType(ScreenType),
  birdTickFPS(BirdTickFPS),
  NumberOfBirdFrames is floor(ElapsedTime * BirdTickFPS),
  0 is (NumberOfBirdFrames mod 1).

tickBirdIfNecessary(GameState, ElapsedTime, TickedGameState):-
  gameState:screenType(GameState, ScreenType),
  shouldTickBird(ScreenType, ElapsedTime),

  gravity(Gravity),
  gameState:bird(GameState, Bird),
  bird:tick(Bird, Gravity, TickedBird),
  gameState:setBird(GameState, TickedBird, TickedGameState),
  !.
tickBirdIfNecessary(GameState, _, GameState).

shouldTickPipeGroups(ScreenType, ElapsedTime):-
  gameState:playingScreenType(ScreenType),
  pipeTickFPS(PipeTickFPS),
  NumberOfPipeFrames is floor(ElapsedTime * PipeTickFPS),
  0 is (NumberOfPipeFrames mod 1).

tickPipeGroupsIfNecessary(GameState, ElapsedTime, TickedGameState):-
  gameState:screenType(GameState, ScreenType),
  shouldTickPipeGroups(ScreenType, ElapsedTime),

  gameState:pipeGroups(GameState, PipeGroups),
  tickAllPipeGroups(PipeGroups, TickedPipeGroups),
  removePipeGroupsIfOutOfScreen(TickedPipeGroups, NewPipeGroups),
  gameState:setPipeGroups(GameState, NewPipeGroups, TickedGameState),
  !.
tickPipeGroupsIfNecessary(PipeGroups, _, PipeGroups).

tickAllPipeGroups([], []).
tickAllPipeGroups([PipeGroup | TailPipeGroups], [TickedPipeGroup | TickedTailPipeGroups]):-
  pipeGroup:tick(PipeGroup, TickedPipeGroup),
  tickAllPipeGroups(TailPipeGroups, TickedTailPipeGroups).

removePipeGroupsIfOutOfScreen([PipeGroup | TailPipeGroups], TailPipeGroups):-
  pipeGroup:originX(PipeGroup, OriginX),
  pipeWidth(Width),
  OriginX + Width =< 0,
  !.
removePipeGroupsIfOutOfScreen(PipeGroups, PipeGroups).

tickScoreIfNecessary(GameState, ElapsedTime, TickedGameState):-
  gameState:screenType(GameState, ScreenType),
  gameState:playingScreenType(ScreenType),

  scoreTickFPS(ScoreTickFPS),
  NumberOfScoreFrames is floor(ElapsedTime * ScoreTickFPS),
  0 is (NumberOfScoreFrames mod 1),
  scoreIncrement(ScoreIncrement),
  gameState:incrementScore(GameState, ScoreIncrement, TickedGameState),
  !.
tickScoreIfNecessary(GameState, _, GameState).
