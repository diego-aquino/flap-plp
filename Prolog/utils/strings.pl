:- module(strings, [lines/2, size/2, chartAt/3, removeLeadingSpaces/2]).

:- use_module(list).

split(String, Separator, StringParts):-
  split_string(String, Separator, '', StringParts).

lines(String, StringLines):-
  split(String, '\n', StringLines).

characters(String, StringCharacters):-
  atom_chars(String, StringCharacters).

size(String, StringSize):-
  string_length(String, StringSize).

chartAt(String, Index, Char):-
  sub_string(String, Index, 1, _, Char).

removeLeadingSpaces(String, StringWithoutLeadingSpaces):-
  characters(String, Characters),
  removeLeadingSpacesFromCharacterList(
    Characters,
    CharactersExcludingLeadingSpaces
  ),
  list:join(CharactersExcludingLeadingSpaces, '', StringWithoutLeadingSpaces).

removeLeadingSpacesFromCharacterList([], []).
removeLeadingSpacesFromCharacterList(
  [Character | TailCharacters],
  CharactersExcludingLeadingSpaces
):-
  (
    Character = ' ' ->
      removeLeadingSpacesFromCharacterList(
        TailCharacters,
        CharactersExcludingLeadingSpaces
      );
    CharactersExcludingLeadingSpaces = [Character | TailCharacters]
  ).

