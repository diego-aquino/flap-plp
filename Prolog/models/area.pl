:- module(area, [createFromLines/2, createFromString/4]).

:- use_module('../utils/strings').
:- use_module(line).

createFromLines(OccupiedLines, Area):-
  Area = area(lines(OccupiedLines)).

createFromString(OriginX, OriginY, String, Area):-
  strings:lines(String, StringLines),
  parseStringLines(OriginX, OriginY, StringLines, OccupiedLines),
  createFromLines(OccupiedLines, Area).
  
merge(Area, AnotherArea, NewArea):-
    occupiedLines(Area, OccupiedLinesFromArea),
    occupiedLines(AnotherArea, OccupiedLinesFromAnotherArea),
    append(OccupiedLinesFromArea, OccupiedLinesFromAnotherArea, OccupiedLines),
    createFromLines(OccupiedLines, NewArea).

parseStringLines(OriginX, OriginY, StringLines, Lines):-
  parseStringLinesRecursive(OriginX, OriginY, StringLines, 0, Lines).

parseStringLinesRecursive(_OriginX, _OriginY, [], _Index, []).
parseStringLinesRecursive(OriginX, OriginY, [StringLine | TailStringLines], Index, [Line | TailLines]):-
  NextIndex is Index + 1,
  parseStringLinesRecursive(OriginX, OriginY, TailStringLines, NextIndex, TailLines),
  LineOriginY is OriginY + Index,
  parseStringLine(OriginX, LineOriginY, StringLine, Line).

parseStringLine(BaseX, BaseY, StringLine, Line):-
  strings:size(StringLine, StringLineLength),
  strings:removeLeadingSpaces(StringLine, StringLineWithoutLeadingSpaces),
  strings:size(StringLineWithoutLeadingSpaces, NumberOfValidCharacters),
  NumberOfLeadingSpaces is StringLineLength - NumberOfValidCharacters,
  OriginX is BaseX + NumberOfLeadingSpaces,
  OriginY = BaseY,
  EndX is OriginX + NumberOfValidCharacters - 1,
  line:create(OriginX, OriginY, EndX, Line).

overlapsWith(Area, AnotherArea, Overlaps):-
  occupiedLines(Area, AreaLines),
  occupiedLines(AnotherArea, AnotherAreaLines),
  (line:anyLinesOverlap(AreaLines, AnotherAreaLines) -> Overlaps = true; Overlaps = false).

occupiedLines(Area, OccupiedLines):-
  Area = area(lines(OccupiedLines)).
