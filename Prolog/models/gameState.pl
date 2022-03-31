:- module(gameState, [ create/6, bird/2, setBird/3, incrementScore/3]).

playingScreenType('playing-screen').
pausedScreenType('paused-screen').
gameOverScreenType('game-over-screen').

create(Bird, PipeGroups, Score, HighestScore, ScreenType, GameState) :-
  isScreenType(ScreenType),
  GameState=gameState(bird(Bird), pipeGroups(PipeGroups), score(Score), highestScore(HighestScore), screenType(ScreenType)).

bird(GameState, Bird) :-
  GameState=gameState(bird(Bird), pipeGroups(_), score(_), highestScore(_), screenType(_)).

pipeGroups(GameState, PipeGroups) :-
  GameState=gameState(bird(_), pipeGroups(PipeGroups), score(_), highestScore(_), screenType(_)).

score(GameState, Score) :-
  GameState=gameState(bird(_), pipeGroups(_), score(Score), highestScore(_), screenType(_)).

highestScore(GameState, HighestScore) :-
  GameState=gameState(bird(_), pipeGroups(_), score(_), highestScore(HighestScore), screenType(_)).

screenType(GameState, ScreenType) :-
  GameState=gameState(bird(_), pipeGroups(_), score(_), highestScore(_), screenType(ScreenType)).

isScreenType(ScreenType) :-
  playingScreenType(ScreenType);
  pausedScreenType(ScreenType);
  gameOverScreenType(ScreenType).

changeScreenType(GameState,NewScreenType,NewGameState):-
  bird(GameState,Bird),
  pipeGroups(GameState,PipeGroups),
  score(GameState,Score),
  highestScore(GameState,HighestScore),
  create(Bird, PipeGroups, Score, HighestScore, NewScreenType, NewGameState).

incrementScore(GameState, Increment, NewGameState) :-
  bird(GameState, Bird),
  pipeGroups(GameState, PipeGroups),
  score(GameState, Score),
  highestScore(GameState, HighestScore),
  screenType(GameState, ScreenType),
  NewScore is Score+Increment,
  NewGameState=gameState(bird(Bird), pipeGroups(PipeGroups), score(NewScore), highestScore(HighestScore), screenType(ScreenType)).

setBird(GameState,NewBird,NewGameState):-
  pipeGroups(GameState,PipeGroups),
  score(GameState,Score),
  highestScore(GameState,HighestScore),
  screenType(GameState,ScreenType),
  create(NewBird, PipeGroups, Score, HighestScore, ScreenType, NewGameState).
