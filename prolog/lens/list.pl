/**
  * FILENAME: lens/term.pl
  * DESCRIPTION: Lists.
  * AUTHORS: José Antonio Riaza Valverde <riaza.valverde@gmail.com>
  * GITHUB: https://github.com/jariazavalverde/prolog-lens
  * UPDATED: 15.05.2021
  *
  **/

:- module(lens_list,
	[ head_/3,
	  tail_/3,
	  nth0_/4,
	  nth1_/4
	]).

:- use_module(lens, [lens_map/3]).

%!  head_(+Functor, +List, -Result)
%
%   Gives access to the head of a list.
head_(F, [A|Xs], Fbx) :-
	call(F, A, Fb),
	lens_map(lens_list:set_nth0_list(0, [A|Xs]), Fb, Fbx).

%!  tail_(+Functor, +List, -Result)
%
%   Gives access to the tail of a list.
tail_(F, [X|A], Fbx) :-
	call(F, A, Fb),
	lens_map(lens_list:mk_list(X), Fb, Fbx).

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

%!  mk_list(+X, +Xs, -XXs)
%
%   Makes a list from two head and tail.
mk_list(X, Xs, [X|Xs]).

%!  set_nth0_list
set_nth0_list(0, [_|Xs], X, [X|Xs]).
set_nth0_list(N, [Y|Xs], X, [Y|Ys]) :-
	succ(M, N),
	set_nth0_list(M, Xs, X, Ys).