/**
  * FILENAME: identity.pl
  * DESCRIPTION: Identity.
  * AUTHORS: José Antonio Riaza Valverde <riaza.valverde@gmail.com>
  * GITHUB: https://github.com/jariazavalverde/prolog-lens
  * UPDATED: 12.05.2021
  *
  **/

:- module(identity,
	[ mk_identity/2,
	  run_identity/2
	]).

%!  mk_const(+Value, -Identity)
%
%   Makes an identity,
mk_identity(X, identity(X)).

%!  get_const(+Identity, -Value)
%
%   Gets an identity.
run_identity(identity(X), X).

%!  lens_map(+Function, +Functor, -Result)
%
%   Checks if a goal can be applied to a structure.
:- multifile(lens_map/3).
lens:lens_map(F, identity(A), identity(B)) :- call(F, A, B).