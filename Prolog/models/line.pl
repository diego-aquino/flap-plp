:- module(line, [create/4]).

create(OriginX, OriginY, EndX, Line):-
  Line = line(originX(OriginX), originY(OriginY), endX(EndX)).

originX(Line, OriginX):-
  Line = line(originX(OriginX), originY(_), endX(_)).

originY(Line, OriginY):-
  Line = line(originX(_), originY(OriginY), endX(_)).

endX(Line, EndX):-
  Line = line(originX(_), originY(_), endX(EndX)).
