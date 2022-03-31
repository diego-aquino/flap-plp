:- module(gameScreen, [render/1]).

:- use_module(terminal).
:- use_module(bird).
:- use_module(pipeGroup).
:- use_module(gameState).
:- use_module('../utils/list').
:- use_module('../utils/strings').

% In the future:
% render(GameState):-
render(GameState):-
  renderPlayingScreen(GameState).

renderPausedScreen().

renderPlayingScreen(GameState):-
  terminal:getSize(TerminalWidth, TerminalHeight),

  ScreenMatrixWidth is TerminalWidth,
  ScreenMatrixHeight is TerminalHeight - 1,
  createEmptyScreenMatrix(ScreenMatrixWidth, ScreenMatrixHeight, EmptyScreenMatrix),

  gameState:bird(GameState, Bird),
  gameState:pipeGroups(GameState, PipeGroups),

  renderBirdToScreenMatrix(Bird, EmptyScreenMatrix, ScreenMatrixWithBird),
  renderPipeGroupsToScreenMatrix(PipeGroups, ScreenMatrixWithBird, ScreenMatrixWithBirdAndPipeGroups),

  printScreenMatrix(ScreenMatrixWithBirdAndPipeGroups).

renderGameOverScreen().

createEmptyScreenMatrix(ScreenMatrixWidth, ScreenMatrixHeight, EmptyScreenMatrix):-
  list:createMatrix(ScreenMatrixWidth, ScreenMatrixHeight, " ", EmptyScreenMatrix).

renderBirdToScreenMatrix(Bird, ScreenMatrix, ScreenMatrixWithBird):-
  bird:originX(Bird, OriginX),
  bird:originY(Bird, OriginY),
  bird:toString(Bird, BirdAsString),
  renderObject(OriginX, OriginY, BirdAsString, ScreenMatrix, ScreenMatrixWithBird).

renderPipeGroupsToScreenMatrix([], ScreenMatrix, ScreenMatrix).
renderPipeGroupsToScreenMatrix([PipeGroup | TailPipeGroups], ScreenMatrix, ScreenMatrixWithPipeGroups):-
  renderPipeGroupsToScreenMatrix(TailPipeGroups, ScreenMatrix, TailRenderedScreenMatrix),
  pipeGroup:originX(PipeGroup, OriginX),
  pipeGroup:originY(PipeGroup, OriginY),
  pipeGroup:toString(PipeGroup, PipeGroupString),
  renderObject(OriginX, OriginY, PipeGroupString, TailRenderedScreenMatrix, ScreenMatrixWithPipeGroups).

renderObject(OriginX, OriginY, ObjectString, ScreenMatrix, RenderedScreenMatrix):-
  widthScreenMatrix(ScreenMatrix, WidthScreenMatrix),
  heightScreenMatrix(ScreenMatrix, HeightScreenMatrix),

  strings:lines(ObjectString, ObjectStringLines),
  objectWidth(ObjectStringLines, ObjectWidth),
  objectHeight(ObjectStringLines, ObjectHeight),

  (
    (
      OriginX >= WidthScreenMatrix;
      (OriginX + ObjectWidth) =< 0;
      OriginY >= HeightScreenMatrix;
      (OriginY + ObjectHeight) =< 0
    ) -> RenderedScreenMatrix = ScreenMatrix;

    renderObjectLines(OriginX, OriginY, ObjectStringLines, ObjectHeight, 0, ScreenMatrix, RenderedScreenMatrix)
  ).

renderObjectLines(_OriginX, _OriginY, _ObjectStringLines, _ObjectHeight, _LineY, [], []).
renderObjectLines(
  OriginX,
  OriginY,
  ObjectStringLines,
  ObjectHeight,
  LineY,
  ScreenMatrix,
  RenderedScreenMatrix
):-
  [ScreenMatrixLine | TailScreenMatrixLines] = ScreenMatrix,

  renderObjectLine(
    OriginX,
    OriginY,
    ObjectStringLines,
    ObjectHeight,
    ScreenMatrixLine,
    0,
    LineY,
    RenderedScreenMatrixLine
  ),

  NextLineY is LineY + 1,
  renderObjectLines(
    OriginX,
    OriginY,
    ObjectStringLines,
    ObjectHeight,
    NextLineY,
    TailScreenMatrixLines,
    TailRenderedScreenMatrixLines
  ),

  RenderedScreenMatrix = [RenderedScreenMatrixLine | TailRenderedScreenMatrixLines].

renderObjectLine(_OriginX, _OriginY, _ObjectStringLines, _ObjectHeight, [], _CellX, _LineY, []):- !.
renderObjectLine(
  OriginX,
  OriginY,
  ObjectStringLines,
  ObjectHeight,
  ScreenMatrixLine,
  CellX,
  LineY,
  RenderedScreenMatrixLine
):-
  LineY >= OriginY,
  LineY < OriginY + ObjectHeight,

  [CellElement | TailCellElements] = ScreenMatrixLine,

  renderObjectLineCell(
    OriginX,
    OriginY,
    ObjectStringLines,
    ObjectHeight,
    CellX,
    LineY,
    CellElement,
    RenderedCellElement
  ),

  NextCellX is CellX + 1,
  renderObjectLine(
    OriginX,
    OriginY,
    ObjectStringLines,
    ObjectHeight,
    TailCellElements,
    NextCellX,
    LineY,
    TailRenderedCellElements
  ),

  RenderedScreenMatrixLine = [RenderedCellElement | TailRenderedCellElements],
  !.
renderObjectLine(
  _OriginX,
  _OriginY,
  _ObjectStringLines,
  _ObjectHeight,
  ScreenMatrixLine,
  _CellX,
  _LineY,
  ScreenMatrixLine
).

renderObjectLineCell(
  OriginX,
  OriginY,
  ObjectStringLines,
  _ObjectHeight,
  CellX,
  LineY,
  _CellElement,
  RenderedCellElement
):-
  CellX >= OriginX,

  ObjectMatchingLineIndex is LineY - OriginY,

  nth0(ObjectMatchingLineIndex, ObjectStringLines, ObjectMatchingLine),
  strings:size(ObjectMatchingLine, ObjectMatchingLineLength),

  ObjectCharacterForCellIndex is CellX - OriginX,
  CellX < OriginX + ObjectMatchingLineLength,

  strings:chartAt(ObjectMatchingLine, ObjectCharacterForCellIndex, RenderedCellElement),
  !.
renderObjectLineCell(
  _OriginX,
  _OriginY,
  _ObjectStringLines,
  _ObjectHeight,
  _CellX,
  _LineY,
  CellElement,
  CellElement
).

objectWidth(ObjectStringLines, ObjectWidth):-
  maxObjectStringLineWidth(ObjectStringLines, ObjectWidth).

objectHeight(ObjectStringLines, ObjectWidth):-
  length(ObjectStringLines, ObjectWidth).

maxObjectStringLineWidth([], 0).
maxObjectStringLineWidth([Line | TailLines], MaxWidth):-
  maxObjectStringLineWidth(TailLines, MaxTailWidth),
  strings:size(Line, LineWidth),
  MaxWidth is max(LineWidth, MaxTailWidth).

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

heightScreenMatrix(ScreenMatrix, Height):- length(ScreenMatrix, Height).

widthScreenMatrix([], 0).
widthScreenMatrix([FirstRow | _], Width):- length(FirstRow, Width).
