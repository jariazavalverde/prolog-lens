/**
  * FILENAME: lens/term.pl
  * DESCRIPTION: Lists.
  * AUTHORS: José Antonio Riaza Valverde <riaza.valverde@gmail.com>
  * GITHUB: https://github.com/jariazavalverde/prolog-lens
  * UPDATED: 15.05.2021
  *
  **/

:- module(lens_list,
	[ nth0_/4,
	  nth1_/4
	]).

:- use_module(lens, [lens_map/3]).

%!  nth0_(+N, +Functor, +List, -Result)
%
%   Gives access to the nth-element of a list.
nth0_(N, F, Xs, Fbx) :-
	nth0(N, Xs, A),
	call(F, A, Fb),
	lens_map(lens_list:set_nth0_list(N, Xs), Fb, Fbx).

%!  nth1_(+N, +Functor, +List, -Result)
%
%   Gives access to the nth-element of a list.
nth1_(N, F, Xs, Fbx) :-
	nth1(M, Xs, A),
	call(F, A, Fb),
	succ(M, N),
	lens_map(lens_list:set_nth0_list(M, Xs), Fb, Fbx).

%!  set_nth0_list
set_nth0_list(0, [_|Xs], X, [X|Xs]).
set_nth0_list(N, [Y|Xs], X, [Y|Ys]) :-
	succ(M, N),
	set_nth0_list(M, Xs, X, Ys).