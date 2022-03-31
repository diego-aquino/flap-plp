:- module(bird,[create/4, jump/3]).

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

jump(Bird, NewVerticalSpeed, JumpedBird):-
  originX(Bird, OriginX),
  originY(Bird, OriginY),
  create(OriginX, OriginY, NewVerticalSpeed, JumpedBird).

getArea(Bird, Area):-
  toString(Bird, String),
  originX(Bird, OriginX),
  originY(Bird, OriginY),
  area:createFromString(OriginX, OriginY, String, Area).

toString(Bird, StringRepresentation):-
  verticalSpeed(Bird, VerticalSpeed),
  (
  VerticalSpeed < 0 -> jumpingString(String);
  VerticalSpeed > 0 -> downString(String);
  VerticalSpeed == 0 -> middleString(String)
  ) -> StringRepresentation = String.


jumpingString(String):-
  String = "   . 7\n // _/".

middleString(String):-
  String = "== (.\n \\\\___\\\\".

downString(String):-
  String = "\\\\ .\n \\\\__\\".
