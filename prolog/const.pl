/**
  * FILENAME: const.pl
  * DESCRIPTION: Const.
  * AUTHORS: José Antonio Riaza Valverde <riaza.valverde@gmail.com>
  * GITHUB: https://github.com/jariazavalverde/prolog-lens
  * UPDATED: 12.05.2021
  *
  **/

:- module(const,
	[ mk_const/2,
	  get_const/2
	]).

%!  mk_const(+Value, -Const)
%
%   Makes a const
mk_const(X, const(X)).

%!  get_const(+Const, -Value)
%
%   Gets a const.
get_const(const(X), X).

%!  lens_map(+Function, +Functor, -Result)
%
%   Checks if a goal can be applied to a structure.
:- multifile lens_map/3.
lens:lens_map(_, const(X), const(X)).