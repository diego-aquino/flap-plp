:- module(controller, [initGameLoop/0]).

:- use_module(gameScreen).
:- use_module(gameState).
:- use_module(terminal).
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

initGameLoop:-
  terminal:hideCursor,
  terminal:startPlayerInputThread,

  bird:create(4, 5, 0, Bird),
  InitialScore = 0,
  HighestScore = 0, % temporarily hardcoded 0
  gameState:playingScreenType(PlayingScreenType),

  gameState:create(Bird, [], InitialScore, HighestScore, PlayingScreenType, InitialGameState),

  run(InitialGameState, 10).

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
  tick(GameStateWithNewPipeGroup, ElapsedTime, TickedGame),

  % Tick
  % Check collisions
  % Save high score

  terminal:moveCursorToOrigin,
  gameScreen:render(TickedGame),

  delayBetweenGameFrames(DelayInSeconds),
  sleep(DelayInSeconds),
  NextElapsedTime is ElapsedTime + DelayInSeconds,
  run(TickedGame, NextElapsedTime).

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
  gameState:screenType(GameState, ScreenType),

  gameState:bird(GameState, Bird),
  tickBirdIfNecessary(Bird, ScreenType, ElapsedTime, TickedBird),
  gameState:setBird(GameState, TickedBird, GameStateWithTickedBird),

  gameState:pipeGroups(GameState, PipeGroups),
  tickPipeGroupsIfNecessary(PipeGroups, ScreenType, ElapsedTime, TickedPipeGroups),
  gameState:setPipeGroups(GameStateWithTickedBird, TickedPipeGroups, TickedGameState).

shouldTickBird(ScreenType, ElapsedTime):-
  birdTickFPS(BirdTickFPS),
  gameState:playingScreenType(ScreenType),
  NumberOfBirdFrames is floor(ElapsedTime * BirdTickFPS),
  0 is (NumberOfBirdFrames mod 1).

tickBirdIfNecessary(Bird, ScreenType, ElapsedTime, TickedBird):-
  shouldTickBird(ScreenType, ElapsedTime),
  gravity(Gravity),
  bird:tick(Bird, Gravity, TickedBird),
  !.
tickBirdIfNecessary(Bird, _, _, Bird).

shouldTickPipeGroups(ScreenType, ElapsedTime):-
  pipeTickFPS(PipeTickFPS),
  gameState:playingScreenType(ScreenType),
  NumberOfPipeFrames is floor(ElapsedTime * PipeTickFPS),
  0 is (NumberOfPipeFrames mod 1).

tickPipeGroupsIfNecessary(PipeGroups, ScreenType, ElapsedTime, NewPipeGroups):-
  shouldTickPipeGroups(ScreenType, ElapsedTime),
  tickAllPipeGroups(PipeGroups, TickedPipeGroups),
  removePipeGroupsIfOutOfScreen(TickedPipeGroups, NewPipeGroups),
  !.
tickPipeGroupsIfNecessary(PipeGroups, _, _, PipeGroups).

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
