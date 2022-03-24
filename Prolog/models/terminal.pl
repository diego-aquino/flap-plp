:- module(terminal, [getTerminalWidth/1, getTerminalHeight/1, resetCursor/0, startPlayerInputThread/0, fetchFromThread/1]).

getTerminalWidth(Width):- tty_size(_, Width).
getTerminalHeight(Height):- tty_size(Height, _).

resetCursor():-
  tty_goto(0, 0).

startPlayerInputThread:-
thread_create(getPlayerInput, Id).

getPlayerInput:-
  getkey(NewInput),
  handlePlayerInput(NewInput),
  getPlayerInput.

getkey(X):-
  ttyflush,
  get_single_char(X).

handlePlayerInput(Input):- thread_send_message(main,Input).

fetchFromThread(Resp):- thread_get_message(main,Resp,[timeout(0)]).
fetchFromThread(Resp):- thread_get_message(main,Resp,[timeout(0)]); Resp = "Nothing".
