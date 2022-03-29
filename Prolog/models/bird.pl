:- module(bird,[create/4]).

:- use_module(area).

create(OriginX, OriginY, VerticalSpeed, Bird) :-
    Bird=bird(originX(OriginX), originY(OriginY), verticalSpeed(VerticalSpeed)).

originX(Bird, OriginX) :-
    Bird=bird(originX(OriginX), originY(_), verticalSpeed(_)).

originY(Bird, OriginY) :-
    Bird=bird(originX(_), originY(OriginY), verticalSpeed(_)).

verticalSpeed(Bird, VerticalSpeed) :-
    Bird=bird(originX(_), originY(_), verticalSpeed(VerticalSpeed)).

tick(Bird, Gravity, NewBird) :-
  originX(Bird, OriginX),
  originY(Bird, OriginY),
  verticalSpeed(Bird, VerticalSpeed),
  NewOriginY is OriginY + floor(VerticalSpeed),
  NewVerticalSpeed is VerticalSpeed + Gravity,
  NewBird = bird(originX(OriginX), originY(NewOriginY), verticalSpeed(NewVerticalSpeed)).

getArea(Bird, Area):-
  toString(String),
  originX(Bird, OriginX),
  originY(Bird, OriginY),
  area:createFromString(OriginX, OriginY, String, Area).

toString(String):-
  String = "== (.\n \\\\___\\\\".
