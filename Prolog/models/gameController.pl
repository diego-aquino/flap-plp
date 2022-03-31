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
  run(Bird, 0).

% Stops program when the exit is typed. Stops the program correctly, but causes some problems afterwards.
haltIfExitKeyWasTyped(CharCode):-
  exitKeyNumber(CharCode),
  terminal:showCursor,
  halt,
  !.
haltIfExitKeyWasTyped(_).

processInput(Bird, CharCode, BirdWithInput):-
  actionKeyNumber(CharCode),
  birdJumpVerticalSpeed(BirdJumpVerticalSpeed),
  bird:jump(Bird, BirdJumpVerticalSpeed, BirdWithInput),
  !.
processInput(Bird, _, Bird).

run(Bird, ElapsedTime):-
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

  setPipeGroupToState()

  processInput(Bird, CharCode, BirdWithInput),
  tick(BirdWithInput, ElapsedTime, TickedBird), % tick(State, ElapsedTime, TickedState)

  % Tick
  % Check collisions
  % Save high score

  terminal:moveCursorToOrigin,
  gameScreen:render(TickedBird),

  delayBetweenGameFrames(DelayInSeconds),
  sleep(DelayInSeconds),
  NextElapsedTime is ElapsedTime + DelayInSeconds,
  run(TickedBird, NextElapsedTime).

shouldCreatePipeGroup(ScreenType, ElapsedTime, Time):- 
  ScreenType = 'playing-screen', 
  ElapsedTime mod Time =:= 0.   

createNewPipeGroup(OriginX, HoleOriginY, PipeGroupHeight, PipeGroup):-
  pipeGroupHoleHeight(HoleHeight),
  pipeGroupOriginY(OriginY),
  pipeWidth(Width),
  TopPipeHeight is HoleOriginY - OriginY,
	BottomPipeHeight is Height - TopPipeHeight - HoleHeight,
	BottomPipeOriginY is OriginY + TopPipeHeight + HoleHeight,
	pipe:create(OriginX, OriginY, Width, TopPipeHeight, "DOWN", TopPipe),
	pipe:create(OriginX, BottomPipeOriginY, Width, BottomPipeHeight, "UP", BottomPipe),
  pipeGroup:create(OriginX, OriginY, TopPipe, BottomPipe, Width, PipeGroupHeight, HoleOriginY, HoleHeight, PipeGroup).


setPipeGroupToState(GameState, ElapsedTime, OriginX, HoleOriginY, PipeGroupHeight, NewGameState):- 
  gameState:screenType(GameState, ScreenType),
  timeBetweenPipeCreations(Time),
  gameState:pipeGroups(GameState, PipeGroups),
  createNewPipeGroup(OriginX, HoleOriginY, PipeGroupHeight, NewPipeGroup),
  append(PipeGroups, [NewPipeGroup], NewPipeGroupList),
  (shouldCreatePipeGroup(ScreenType, ElapsedTime, Time) -> gameState:setPipeGroups(GameState, NewPipeGroupList, NewGameState);
  NewGameState = GameState).

tick(Bird, ElapsedTime, TickedBird):-
  tickBirdIfNecessary(Bird, ElapsedTime, TickedBird).

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

tickAllPipeGroups(_,[],[]).
tickAllPipeGroups([Head|Tail],)


removePipeGroupsIfNecessary([Head|Tail], NewPipeGroupList):-
  length([Head|Tail], Length),
  pipeGroup:originX(Head, OriginX),
  pipeWidth(Width),
  (Length > 0, OriginX + Widht =< 0 -> NewPipeGroupList = Tail;
  NewPipeGroupList = [Head|Tail]).
  
      

