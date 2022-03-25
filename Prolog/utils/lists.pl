:- module(lists, [join/3, createMatrix/4, createList/3]).

join(List, Separator, JoinedList):-
  atomic_list_concat(List, Separator, JoinedList).

createMatrix(_, 0, _, []).
createMatrix(Width, Height, InitialValue, [Row | NextRows]):-
  NextHeight is Height - 1,
  createMatrix(Width, NextHeight, InitialValue, NextRows),
  createList(Width, InitialValue, Row).

createList(0, _, []).
createList(Length, InitialValue, [InitialValue | NextList]):-
  NextLength is Length - 1,
  createList(NextLength, InitialValue, NextList).
