:- module(gameScreen, [render/1]).

:- use_module(terminal).
:- use_module(bird).
:- use_module(gameState).
:- use_module('../utils/list').
:- use_module('../utils/strings').

render(GameState):-
  gameState:screenType(GameState,ScreenType),
  renderScreen(ScreenType,GameState).

renderScreen('paused-screen',GameState):-
  terminal:getSize(TerminalWidth, TerminalHeight),

  ScreenMatrixWidth is TerminalWidth,
  ScreenMatrixHeight is TerminalHeight - 1,
  createEmptyScreenMatrix(ScreenMatrixWidth, ScreenMatrixHeight, EmptyScreenMatrix),

  gameState:highestScore(GameState,HighestScore),
  string_concat("High Score: ", HighestScore, LastTextLine),
  
  renderTextToScreenMatrix(["Welcome to Flap-PLP", "Press <Enter> to flap your wings", LastTextLine],EmptyScreenMatrix,ScreenMatrixWithText),
  
  printScreenMatrix(ScreenMatrixWithText).

renderScreen('game-over-screen',GameState):-
  terminal:getSize(TerminalWidth, TerminalHeight),

  ScreenMatrixWidth is TerminalWidth,
  ScreenMatrixHeight is TerminalHeight - 1,
  createEmptyScreenMatrix(ScreenMatrixWidth, ScreenMatrixHeight, EmptyScreenMatrix),

  gameState:highestScore(GameState,HighestScore),
  string_concat("High Score: ", HighestScore, LastTextLine),
  
  renderTextToScreenMatrix(["Game Over", "Press <Enter> to play again", LastTextLine],EmptyScreenMatrix,ScreenMatrixWithText),
  
  printScreenMatrix(ScreenMatrixWithText).

renderScreen('playing-screen',GameState):-
  terminal:getSize(TerminalWidth, TerminalHeight),

  ScreenMatrixWidth is TerminalWidth,
  ScreenMatrixHeight is TerminalHeight - 1,
  createEmptyScreenMatrix(ScreenMatrixWidth, ScreenMatrixHeight, EmptyScreenMatrix),

  gameState:bird(GameState,Bird),
  renderBirdToScreenMatrix(Bird, EmptyScreenMatrix, ScreenMatrixWithBird),
  
  gameState:score(GameState,Score),
  renderScoreToScreenMatrix(Score,ScreenMatrixWithBird,ScreenMatrixWithScore),
  printScreenMatrix(ScreenMatrixWithScore).

renderScoreToScreenMatrix(Score,ScreenMatrix,ScreenMatrixWithScore):-
  string_concat("Score: ",Score,ScoreText),
  atom_length(ScoreText, TextLength),
  widthScreenMatrix(ScreenMatrix,Width),
  OriginX is (Width - TextLength) // 2,
  renderObject(OriginX, 0, ScoreText, ScreenMatrix, ScreenMatrixWithScore).

createEmptyScreenMatrix(ScreenMatrixWidth, ScreenMatrixHeight, EmptyScreenMatrix):-
  list:createMatrix(ScreenMatrixWidth, ScreenMatrixHeight, " ", EmptyScreenMatrix).

renderBirdToScreenMatrix(Bird, ScreenMatrix, ScreenMatrixWithBird):-
  bird:originX(Bird, OriginX),
  bird:originY(Bird, OriginY),
  bird:toString(Bird, BirdAsString),
  renderObject(OriginX, OriginY, BirdAsString, ScreenMatrix, ScreenMatrixWithBird).

renderTextToScreenMatrix([Text|[]],EmptyScreenMatrix,ScreenMatrixWithText):-
  atom_length(Text, TextLength),
  widthScreenMatrix(EmptyScreenMatrix,Width),
  heightScreenMatrix(EmptyScreenMatrix,Height),
  OriginX is (Width - TextLength) // 2,
  OriginY is Height // 2,
  renderObject(OriginX, OriginY, Text,EmptyScreenMatrix,ScreenMatrixWithText).

renderTextToScreenMatrix([TextLine|RemainingTextLines],EmptyScreenMatrix,ScreenMatrixWithText):-
  length(RemainingTextLines,RemainingLinesLength),
  Spacing is RemainingLinesLength * 2,
  renderTextToScreenMatrixRecursive(TextLine,Spacing,EmptyScreenMatrix,ScreenMatrixWithRecursiveText),
  renderTextToScreenMatrix(RemainingTextLines,ScreenMatrixWithRecursiveText,ScreenMatrixWithText).

renderTextToScreenMatrixRecursive(TextLine,Spacing,EmptyScreenMatrix,ScreenMatrixWithText):-
  atom_length(TextLine, TextLength),
  widthScreenMatrix(EmptyScreenMatrix,Width),
  heightScreenMatrix(EmptyScreenMatrix,Height),
  OriginX is (Width - TextLength) // 2,
  OriginY is (Height // 2) - Spacing,
  renderObject(OriginX,OriginY,TextLine,EmptyScreenMatrix,ScreenMatrixWithText).

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
