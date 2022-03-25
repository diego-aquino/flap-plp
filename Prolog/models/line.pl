:- module(line, [create/4]).

create(OriginX, OriginY, EndX, Line):-
  Line = line(originX(OriginX), originY(OriginY), endX(EndX)).

originX(Line, OriginX):-
  Line = line(originX(OriginX), originY(_), endX(_)).

originY(Line, OriginY):-
  Line = line(originX(_), originY(OriginY), endX(_)).

endX(Line, EndX):-
  Line = line(originX(_), originY(_), endX(EndX)).

overlapsWith(Line, AnotherLine):-
  originX(Line, LineOriginX),
  originY(Line, LineOriginY),
  endX(Line, LineEndX),
  originX(AnotherLine, AnotherLineOriginX),
  originY(AnotherLine, AnotherLineOriginY),
  endX(AnotherLine, AnotherLineEndX),
  LineOriginY = AnotherLineOriginY,
  (
    LineOriginX =< AnotherLineOriginX ->
      LineEndX >= AnotherLineOriginX;
      AnotherLineEndX >= LineOriginX
  ).

overlapsWithAny(_Line, []):- false.
overlapsWithAny(Line, [AnotherLine | TailLines]):-
  overlapsWith(Line, AnotherLine);
  overlapsWithAny(Line, TailLines).

anyLinesOverlap([], _AnotherLines):- false.
anyLinesOverlap([Line | TailLines], AnotherLines):-
  overlapsWithAny(Line, AnotherLines);
  anyLinesOverlap(TailLines, AnotherLines).
