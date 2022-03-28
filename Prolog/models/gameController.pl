:- module(controller,[initGameLoop/0]).
:- use_module(terminal).
:- use_module(bird).
:- use_module("../utils/list").

initGameLoop:-
    bird:testMethodBird("Ayo bird here"),nl,
    list:testMethodLists("Sup lists is ehre as well"),nl,
    terminal:startPlayerInputThread,
    run(0,0).


checkShouldExitGame(113):- halt. %Stops program when 'q' is typed. Stops the program correctly, but causes some problems afterwards
checkShouldExitGame(_).

processInput(CurrentState,13,StateWithInput):- StateWithInput is CurrentState + 1,!.
processInput(CurrentState,_,CurrentState).

run(CurrentState,Time):-
    terminal:fetchFromThread(Input),
    checkShouldExitGame(Input),
    %terminal:getTerminalHeight(Height), This methods are commented
    %terminal:getTerminalWidth(Width),

    %Change pipes

    processInput(CurrentState,Input,StateWithInput),

    %Tick

    %Check collisions

    %Save high score

    NextTime is Time + 1,
    sleep(1),
    Message = "Pressed Enter this amount: " + StateWithInput,
    write(Message),nl,
    ttyflush,
    run(StateWithInput,NextTime).
