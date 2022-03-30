:- module(gameState,
          [ create/6
          ]).

playingScreenType('playing-screen').
pausedScreenType('paused-screen').
gameOverScreenType('game-over-screen').

create(Bird, PipeGroups, Score, HighestScore, ScreenType, GameState) :-
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

setScreenType(ScreenType) :-
    playingScreenType(ScreenType),
    pausedScreenType(ScreenType),
    gameOverScreenType(ScreenType).
