/**
  * FILENAME: combinators.pl
  * DESCRIPTION: Combinators.
  * AUTHORS: José Antonio Riaza Valverde <riaza.valverde@gmail.com>
  * GITHUB: https://github.com/jariazavalverde/prolog-lens
  * UPDATED: 15.05.2021
  *
  **/

:- module(combinators,
	[ '*'/4,
	  id/2,
	  const/3,
	  flip/4
	]).

%!  first(+Functor, +Tuple, -Result)
%
%   Function composition.
'*'(G, F, X, Z) :-
	call(F, X, Y),
	call(G, Y, Z).

%!  flip(+Function, +Arg1, +Arg2, -Ouput)
%
%   Flip function.
flip(F, X, Y, Z) :- call(F, Y, X, Z).

%!  id(+Value, -Value)
%
%   Identity function.
id(X, X).

%!  const(+Value, ?Ignore, -Value)
%
%   Constant function.
const(X, _, X).
