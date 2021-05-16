/**
  * FILENAME: lens/term.pl
  * DESCRIPTION: Compound terms.
  * AUTHORS: José Antonio Riaza Valverde <riaza.valverde@gmail.com>
  * GITHUB: https://github.com/jariazavalverde/prolog-lens
  * UPDATED: 15.05.2021
  *
  **/

:- module(lens_term,
	[ functor_/3,
	  args_/3
	]).

:- use_module(lens, [lens_map/3]).

%!  functor_(+Functor, +Term, -Result)
%
%   Gives access to the functor of a compound term.
functor_(F, T, Fbx) :-
	functor(T, A, _),
	call(F, A, Fb),
    lens_map(lens_term:set_functor(T), Fb, Fbx).

%!  args_(+Functor, +Term, -Result)
%
%   Gives access to the arguments of a compound term.
args_(F, T, Fbx) :-
	T =.. [_|A],
	call(F, A, Fb),
    lens_map(lens_term:set_arguments(T), Fb, Fbx).

%!  set_functor
set_functor(T, F, Tf) :-
	T =.. [_|Args],
	Tf =.. [F|Args].

%!  set_arguments
set_arguments(T, Args, Targs) :-
	T =.. [F|_],
	Targs =.. [F|Args].