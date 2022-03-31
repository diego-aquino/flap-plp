:- module(gameState, [create/6, bird/2, setBird/3, pipeGroups/2, setPipeGroups/3, incrementScore/3, playingScreenType/1, pausedScreenType/1, gameOverScreenType/1]).

playingScreenType('playing-screen').
pausedScreenType('paused-screen').
gameOverScreenType('game-over-screen').

create(Bird, PipeGroups, Score, HighestScore, ScreenType, GameState) :-
  isScreenType(ScreenType),
  GameState=gameState(bird(Bird), pipeGroups(PipeGroups), score(Score), highestScore(HighestScore), screenType(ScreenType)).

bird(GameState, Bird) :-
  GameState=gameState(bird(Bird), pipeGroups(_), score(_), highestScore(_), screenType(_)).

setBird(GameState, NewBird, NewGameState) :-
  pipeGroups(GameState, PipeGroups),
  score(GameState, Score),
  highestScore(GameState, HighestScore),
  screenType(GameState, ScreenType),
  NewGameState = gameState(
    bird(NewBird), pipeGroups(PipeGroups), score(Score), highestScore(HighestScore), screenType(ScreenType)
  ).

pipeGroups(GameState, PipeGroups):-
  GameState=gameState(bird(_), pipeGroups(PipeGroups), score(_), highestScore(_), screenType(_)).

setPipeGroups(GameState, NewPipeGroups, NewGameState):-
  bird(GameState, Bird),
  score(GameState, Score),
  highestScore(GameState, HighestScore),
  screenType(GameState, ScreenType),
  NewGameState = gameState(
    bird(Bird), pipeGroups(NewPipeGroups), score(Score), highestScore(HighestScore), screenType(ScreenType)
  ).

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

setPipeGroupToState(GameState, PipeGroupList, NewGameState):-
  bird(GameState, Bird),
  score(GameState, Score),
  highestScore(GameState, HighestScore),
  screenType(GameState, ScreenType),
  NewGameState = gameState(bird(Bird), pipeGroups(PipeGroupList), score(Score), highestScore(HighestScore), screenType(ScreenType)).

incrementScore(GameState, Increment, NewGameState) :-
  bird(GameState, Bird),
  pipeGroups(GameState, PipeGroups),
  score(GameState, Score),
  highestScore(GameState, HighestScore),
  screenType(GameState, ScreenType),
  NewScore is Score+Increment,
  NewGameState=gameState(bird(Bird), pipeGroups(PipeGroups), score(NewScore), highestScore(HighestScore), screenType(ScreenType)).
