/**
  * FILENAME: lens.pl
  * DESCRIPTION: Lenses.
  * AUTHORS: José Antonio Riaza Valverde <riaza.valverde@gmail.com>
  * GITHUB: https://github.com/jariazavalverde/prolog-lens
  * UPDATED: 12.05.2021
  *
  **/

:- module(lens,
	[ view/3,
	  over/4,
	  set/4,
	  lens_map/3
	]).

%!  lens_map(+Function, +Functor, -Result)
%
%   Checks if a goal can be applied to a structure.
:- multifile lens_map/3.

:- use_module(const, [mk_const/2, get_const/2]).
:- use_module(identity, [mk_identity/2, run_identity/2]).
:- use_module(combinators, ['*'/4, const/3]).

%!  view(+Lense, +Source, -Value)
%
%   view(L, S, X) gets a value X out of a structure S using a getter L.
view(L, S, X) :-
	call(L, const:mk_const, S, C),
	get_const(C, X).

%!  view(+Lense, +Function, +SourceIn, -SourceOut)
%
%   view(L, F, S, R) applies a function F to the target S using a setter L.
over(L, F, S, R) :-
	call(L, combinators:'*'(identity:mk_identity, F), S, I),
	run_identity(I, R).

%!  view(+Lense, +Value, +SourceIn, -SourceOut)
%
%   view(L, X, S, R) assigns a value X to the target target S using a setter L.
set(L, X, S, R) :- over(L, const(X), S, R).