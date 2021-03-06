:- module(pipeGroup, [create/7, originX/2, originY/2, tick/2, toString/2]).

:- use_module(area).
:- use_module(pipe).
:- use_module('../utils/list').

create(OriginX, OriginY, Width, Height, HoleOriginY, HoleHeight, PipeGroup):-
	TopPipeHeight is HoleOriginY - OriginY,
	BottomPipeHeight is Height - TopPipeHeight - HoleHeight,
	BottomPipeOriginY is OriginY + TopPipeHeight + HoleHeight,
	pipe:create(OriginX, OriginY, Width, TopPipeHeight, "DOWN", TopPipe),
	pipe:create(OriginX, BottomPipeOriginY, Width, BottomPipeHeight, "UP", BottomPipe),
	PipeGroup = pipeGroup(
    originX(OriginX) , originY(OriginY), topPipe(TopPipe), bottomPipe(BottomPipe), width(Width), holeHeight(HoleHeight)
  ).

originX(PipeGroup, OriginX):-
	PipeGroup = pipeGroup(originX(OriginX) , originY(_), topPipe(_), bottomPipe(_), width(_), holeHeight(_)).

originY(PipeGroup, OriginY):-
	PipeGroup = pipeGroup(originX(_) , originY(OriginY), topPipe(_), bottomPipe(_), width(_), holeHeight(_)).

topPipe(PipeGroup, TopPipe):-
	PipeGroup = pipeGroup(originX(_) , originY(_), topPipe(TopPipe), bottomPipe(_), width(_), holeHeight(_)).

bottomPipe(PipeGroup, BottomPipe):-
	PipeGroup = pipeGroup(originX(_) , originY(_), topPipe(_), bottomPipe(BottomPipe), width(_), holeHeight(_)).

width(PipeGroup, Width):-
	PipeGroup = pipeGroup(originX(_) , originY(_), topPipe(_), bottomPipe(_), width(Width), holeHeight(_)).

holeHeight(PipeGroup, HoleHeight):-
	PipeGroup = pipeGroup(originX(_) , originY(_), topPipe(_), bottomPipe(_), width(_), holeHeight(HoleHeight)).

tick(PipeGroup, TickedPipeGroup):-
	topPipe(PipeGroup, TopPipe),
	bottomPipe(PipeGroup, BottomPipe),
	width(PipeGroup, Width),
	holeHeight(PipeGroup, HoleHeight),

  pipe:tick(TopPipe,TickedTopPipe),
  pipe:tick(BottomPipe,TickedBottomPipe),
  pipe:originX(TickedTopPipe, NewOriginX),
  pipe:originY(TickedTopPipe, NewOriginY),

	TickedPipeGroup = pipeGroup(
    originX(NewOriginX), originY(NewOriginY), topPipe(TickedTopPipe), bottomPipe(TickedBottomPipe), width(Width), holeHeight(HoleHeight)
  ).

getArea(PipeGroup, Area):-
	topPipe(PipeGroup, TopPipe),
	bottomPipe(PipeGroup, BottomPipe),
	pipe:getArea(TopPipe, TopPipeArea),
	pipe:getArea(BottomPipe, BottomPipeArea),
	area:merge(TopPipeArea, BottomPipeArea, Area).

toString(PipeGroup, String):-
	topPipe(PipeGroup, TopPipe),
	bottomPipe(PipeGroup, BottomPipe),
	holeHeight(PipeGroup, HoleHeight),
	width(PipeGroup, Width),
	pipe:toString(TopPipe, TopPipeAsString),
	pipe:toString(BottomPipe, BottomPipeAsString),
	list:createList(Width, " ", HoleLine),
  list:join(HoleLine, "", HoleLineAsString),
	list:createList(HoleHeight, HoleLineAsString, HoleList),
	list:join(HoleList, '\n', Hole),
	list:join([TopPipeAsString, Hole, BottomPipeAsString], '\n', String).
