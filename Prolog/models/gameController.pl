:- module(controller, [initGameLoop/0]).

:- use_module(gameScreen).
:- use_module(terminal).
:- use_module(bird).
:- use_module('../utils/list').

exitKeyNumber(113). % Key: Q
actionKeyNumber(13). % Key: Enter

gameFPS(20).
delayBetweenGameFrames(DelayInSeconds):-
  gameFPS(GameFPS),
  DelayInSeconds is 1 / GameFPS.

birdTickFPS(20).

gravity(0.2).

initGameLoop:-
  terminal:hideCursor,
  terminal:startPlayerInputThread,
  bird:create(4, 5, 0, Bird),
  run(Bird, 0).

% Stops program when the exit is typed. Stops the program correctly, but causes some problems afterwards.
haltIfExitKeyWasTyped(KeyNumber):-
  exitKeyNumber(KeyNumber),
  terminal:showCursor,
  halt,
  !.
haltIfExitKeyWasTyped(_).

processInput(Bird, KeyNumber, Bird):-
  actionKeyNumber(KeyNumber),
  % Enter was pressed. Process input...
  !.
processInput(Bird, _, Bird).

run(Bird, ElapsedTime):-
  terminal:fetchFromThread(Input),
  haltIfExitKeyWasTyped(Input),

  % terminal:getTerminalHeight(Height), This methods are commented
  % terminal:getTerminalWidth(Width),

  % Change pipes

  processInput(Bird, Input, BirdWithInput),
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
