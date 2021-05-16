/**
  * FILENAME: lens/tuple.pl
  * DESCRIPTION: Tuples.
  * AUTHORS: José Antonio Riaza Valverde <riaza.valverde@gmail.com>
  * GITHUB: https://github.com/jariazavalverde/prolog-lens
  * UPDATED: 15.05.2021
  *
  **/

:- module(lens_tuple,
	[ fst_/3,
	  snd_/3,
	  at_/4
	]).

:- use_module(lens, [lens_map/3]).

%!  fst_(+Functor, +Tuple, -Result)
%
%   Gives access to the first field of a tuple.
fst_(F, (A,X), Fbx) :-
	call(F, A, Fb),
	lens_map(lens_tuple:set_nth_tuple(0, (A,X)), Fb, Fbx).

%!  snd_(+Functor, +Tuple, -Result)
%
%   Gives access to the second field of a tuple.
snd_(F, (X,A), Fbx) :-
	call(F, A, Fb),
	lens_map(lens_tuple:mk_tuple(X), Fb, Fbx).

%!  at_(+N, +Functor, +Tuple, -Result)
%
%   Gives access to the nth field of a tuple.
at_(N, F, T, Fbx) :-
	N >= 0,
	get_nth_tuple(N, T, A),
	call(F, A, Fb),
	lens_map(lens_tuple:set_nth_tuple(N, T), Fb, Fbx).

%!  mk_tuple(+A, +B, -AB)
%
%   Makes a tuple from two elements.
mk_tuple(A, B, (A,B)).

%!  get_nth_tuple
get_nth_tuple(0, (A, _), A) :- !.
get_nth_tuple(0, A, A) :- !.
get_nth_tuple(N, (_, B), A) :-
	succ(M, N),
	get_nth_tuple(M, B, A).

%!  set_nth_tuple
set_nth_tuple(0, (_,B), X, (X,B)) :- !.
set_nth_tuple(0, _, X, X) :- !.
set_nth_tuple(N, (A,B), X, (A,C)) :-
	succ(M, N),
	set_nth_tuple(M, B, X, C).
