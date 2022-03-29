:- module(pipe,[create/6]).

:- use_module(area).
:- use_module('../utils/list').

create(OriginX, OriginY, Width, Heigth, Direction, Pipe) :-
	Pipe = pipe(originX(OriginX), originY(OriginY), width(Width), height(Height), direction(Direction)).

originX(Pipe, OriginX):-
	Pipe = pipe(originX(OriginX), originY(_), width(_), height(_), direction(_)).

originY(Pipe, OriginY):-
	Pipe = pipe(originX(_), originY(OriginY), width(_), height(_), direction(_)).

width(Pipe, Width):-
	Pipe = pipe(originX(_), originY(_), width(Width), height(_), direction(_)).

height(Pipe, Height):-
	Pipe = pipe(originX(_), originY(_), width(_), height(Height), direction(_)).

direction(Pipe, Direction):-
	Pipe = pipe(originX(_), originY(_), width(_), height(_), direction(Direction)).

tick(Pipe, NewPipe):-
	originX(Pipe, OriginX),
	originY(Pipe, OriginY),
	width(Pipe, Width),
	height(Pipe, Height),
	direction(Pipe, Direction),
	NewOriginX is OriginX - 1,
	NewPipe = pipe(originX(NewOriginX), originY(OriginY), width(Width), height(Height), direction(Direction)).

getArea(Pipe, Area):-
	toString(Pipe, String),
	originX(Pipe, OriginX),
	originY(Pipe, OriginY),
	area:createFromString(OriginX, OriginY, String, Area).

toString(Pipe, String):-
	direction(Pipe, Direction),
	(Direction = "DOWN" -> toDownString(Pipe, String);
	toUpString(Pipe,String)).

toDownString(Pipe, ToDownString):-
	toUpString(Pipe,ToUpStringPipe),
	list:join(List, nl, toUpStringPipe),
	reverse(List, ReversedList),
	list:join(ReversedList, nl, ToDownString).

toUpString(Pipe, toUpString):-
	toStringLines(Pipe,StringLines),
	list:join(StringLines, nl, toUpString).

toStringLines(Pipe, StringLines):-
	width(Pipe, Width),
	height(Pipe, Height),
	list:createList(Width, '#', Line),
	list:createList(Height, Line, StringLines).
