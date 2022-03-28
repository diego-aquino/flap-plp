:- module(gameScreen, [render/0]).

:- use_module(terminal).
:- use_module('../utils/list').
:- use_module('../utils/strings').

% In the future:
% render(GameState):-
render():-
  renderPlayingScreen().

renderPausedScreen().

renderPlayingScreen():-
  terminal:getSize(TerminalWidth, TerminalHeight),

  ScreenMatrixWidth is TerminalWidth,
  ScreenMatrixHeight is TerminalHeight - 1,
  createEmptyScreenMatrix(ScreenMatrixWidth, ScreenMatrixHeight, EmptyScreenMatrix),

  printScreenMatrix(EmptyScreenMatrix).

renderGameOverScreen().

createEmptyScreenMatrix(ScreenMatrixWidth, ScreenMatrixHeight, EmptyScreenMatrix):-
  list:createMatrix(ScreenMatrixWidth, ScreenMatrixHeight, " ", EmptyScreenMatrix).

printScreenMatrix(ScreenMatrix):-
  formatScreenMatrixToRender(ScreenMatrix, FormattedScreenMatrix),
  write(FormattedScreenMatrix),
  ttyflush.

formatScreenMatrixToRender(ScreenMatrix, FormattedScreenMatrix):-
  formatScreenMatrixRows(ScreenMatrix, FormattedRows),
  strings:join(FormattedRows, '\n', FormattedScreenMatrix).

formatScreenMatrixRows([], []).
formatScreenMatrixRows([Row | TailRows], [FormattedRow | FormattedTailRows]):-
  formatScreenMatrixRows(TailRows, FormattedTailRows),
  strings:join(Row, '', FormattedRow).
