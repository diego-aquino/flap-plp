:- module(lists, [join/3]).

join(List, Separator, JoinedList):-
  atomic_list_concat(List, Separator, JoinedList).
