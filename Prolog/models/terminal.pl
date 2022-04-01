:- module(terminal, [
  getWidth/1,
  getHeight/1,
  moveCursorToOrigin/0,
  hideCursor/0,
  showCursor/0,
  startPlayerInputThread/0,
  fetchFromThread/1
]).

ansiCodeShowCursor('\x1b[?25h').
ansiCodeHideCursor('\x1b[?25l').

getSize(Width, Height):- tty_size(Height, Width).
getWidth(Width):- tty_size(_, Width).
getHeight(Height):- tty_size(Height, _).

moveCursorToOrigin():-
  tty_goto(0, 0).

showCursor():-
  ansiCodeShowCursor(ShowCursorCode),
  write(ShowCursorCode).

hideCursor():-
  ansiCodeHideCursor(HideCursorCode),
  write(HideCursorCode).

startPlayerInputThread:-
  prompt(_, ''),
  thread_create(getPlayerInput, _Id).

getPlayerInput:-
  getKey(CharCode),
  handlePlayerInput(CharCode),
  getPlayerInput.

getKey(X):-
  ttyflush,
  get_single_char(X).

handlePlayerInput(CharCode):-
  thread_send_message(main, CharCode).

fetchFromThread(CharCode):-
  thread_get_message(main, CharCode, [timeout(0)]),
  !.
fetchFromThread('').
