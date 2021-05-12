/**
  * FILENAME: lens/tuple.pl
  * DESCRIPTION: Tuples.
  * AUTHORS: José Antonio Riaza Valverde <riaza.valverde@gmail.com>
  * GITHUB: https://github.com/jariazavalverde/prolog-lens
  * UPDATED: 12.05.2021
  *
  **/

:- module(tuple,
	[ first/3,
	  second/3
	]).

:- use_module(lens, [lens_map/3]).

%!  first(+Functor, +Tuple, -Result)
%
%   Gives access to the first field of a tuple.
first(F, (A,X), Fbx) :-
	call(F, A, Fb),
    lens_map(tuple:mk_tuple2(X), Fb, Fbx).

%!  second(+Functor, +Tuple, -Result)
%
%   Gives access to the second field of a tuple.
second(F, (X,A), Fbx) :-
	call(F, A, Fb),
    lens_map(tuple:mk_tuple(X), Fb, Fbx).

%!  mk_tuple(+A, +B, -AB)
%
%   Makes a tuple from two elements.
mk_tuple(A, B, (A,B)).

%!  mk_tuple2(+A, +B, -BA)
%
%   Makes a tuple from two elements.
mk_tuple2(A, B, (B,A)).