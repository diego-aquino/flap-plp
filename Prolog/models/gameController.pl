:- module(controller, [initGameLoop/0]).

:- use_module(gameScreen).
:- use_module(gameState).
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

msInASecond(1000000).

pipeTickFPS(20).
pipeWidth(5).
pipeGroupOriginY(0).
pipeGroupHoleHeight(10).
timeBetweenPipeCreations(2000000).

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

  terminal:getHeight(Height), 
  terminal:getWidth(Width),
  pipeGroupHoleHeight(HoleHeight),
  Base = 3,
  Max is Height - HoleHeight - 5,
  random(Base, Max, HoleOriginY),

  pipeGroupOriginY(PipeGroupOriginY),
  PipeGroupHeight is Height - PipeGroupOriginY - 2,
  PipeGroupOriginX is Width + 1, 

  setPipeGroupToState(GameState, ElapsedTime, PipeGroupOriginX, HoleOriginY, PipeGroupHeight, NewGameState),

  processInput(NewGameState, CharCode, GameStateWithInput),
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

shouldCreatePipeGroup(ScreenType, ElapsedTime, Time):- 
  ScreenType = 'playing-screen', 
  ElapsedTime mod Time =:= 0.   

createNewPipeGroup(OriginX, HoleOriginY, PipeGroupHeight, PipeGroup):-
  pipeGroupHoleHeight(HoleHeight),
  pipeGroupOriginY(OriginY),
  pipeWidth(Width),
  pipeGroup:create(OriginX, OriginY, Width, PipeGroupHeight, HoleOriginY, HoleHeight, PipeGroup).


setPipeGroupToState(GameState, ElapsedTime, OriginX, HoleOriginY, PipeGroupHeight, NewGameState):- 
  gameState:screenType(GameState, ScreenType),
  timeBetweenPipeCreations(Time),
  gameState:pipeGroups(GameState, PipeGroups),
  createNewPipeGroup(OriginX, HoleOriginY, PipeGroupHeight, NewPipeGroup),
  append(PipeGroups, [NewPipeGroup], NewPipeGroupList),
  (shouldCreatePipeGroup(ScreenType, ElapsedTime, Time) -> gameState:setPipeGroups(GameState, NewPipeGroupList, NewGameState);
  NewGameState = GameState).

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

shouldTickPipeGroups(ScreenType, ElapsedTime, PipeTickFPS, MsInASecond):- 
  ScreenType = 'playing-screen', 
  ElapsedTime mod PipeTickFPS // MsInASecond =:= 0.   

tickPipeGroupsIfNecessary(GameState, ElapsedTime, NewGameState):- 
  gameState:pipeGroups(GameState, PipeGroups),
  gameState:screenType(GameState, ScreenType),
  pipeTickFPS(TickFPS),
  msInASecond(MsInASecond),
  tickAllPipeGroups(PipeGroups, TickedPipeGroups),
  removePipeGroupsIfNecessary(TickedPipeGroups, NewPipeGroupList),
  (shouldTickPipeGroups(ScreenType, ElapsedTime, MsInASecond) -> gameState:setPipeGroups(GameState, NewPipeGroupList, NewGameState);
  NewGameState = GameState). 
tickPipeGroupsIfNecessary(GameState, _, NewGameState).

tickAllPipeGroups([],[]).
tickAllPipeGroups([Head|Tail], [TickedHead|TickedTail]):-
  pipeGroup:tick(Head,TickedHead),
  tickAllPipeGroups(Tail, TickedTail).

removePipeGroupsIfNecessary([Head|Tail], NewPipeGroupList):-
  length([Head|Tail], Length),
  pipeGroup:originX(Head, OriginX),
  pipeWidth(Width),
  (Length > 0, OriginX + Widht =< 0 -> NewPipeGroupList = Tail;
  NewPipeGroupList = [Head|Tail]).
  
      
