/*
Copyright (C) Peter Ljunglöf

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utilities by Peter Ljunglöf
%% Time-stamp: "2006-04-25, 23:31"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(utilities, [ inbetween/3,
		       update_arg/4,
		       repeat/1,
		       countall/2,
		       count_clauses/2,
		       random/3,
		       random_goal/1,
		       random_member/2,
		       remove_clauses/1,
		       runtime/1,
		       error/3,
		       trace_runtime/2
		     ]).

:- meta_predicate
	remove_clauses(:),
	remove_clauses(:, :),
	trace_runtime(+, :).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% comment out all but one of these:

%% :- include(utilities_swi).
%% :- include(utilities_sicstus).

:- ( current_prolog_flag(version, V), atom_concat('SICStus', _, V) ->
       ensure_loaded(utilities_sicstus)
   ;
       ensure_loaded(utilities_swi)
   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% all integers between From and To, on backtracking

%% inbetween/3
%% prolog variant specific


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% updating an argument of a compound term

%% update_arg/4
%% prolog variant specific


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% repeating a goal a given number of times

repeat(N) :-
	inbetween(1, N, _).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% getting the runtime

runtime(T) :-
	statistics(runtime, [T, _]).

trace_runtime(Topic, Goal) :-
	format(user_error, '[~w, starting]\n', [Topic]),
	runtime(T0),
	Goal,
	runtime(T1),
	T is T1-T0,
	format(user_error, '[~w, runtime: ~3d s]\n', [Topic, T]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% remove all clauses of a predicate

remove_clauses(Mod:Pred) :-
	!, ( Pred = _/_ ->
	       remove_clauses(Mod:Pred, Mod:_)
	   ;
	       remove_clauses(Mod:_, Mod:Pred)
	   ).
remove_clauses(Pred) :-
	( Pred = _/_ ->
	    remove_clauses(Pred, _)
	;
	    remove_clauses(_, Pred)
	).

%% abolish/1 is faster than retractall/1;
%% retractall/1 is needed to make the predicate dynamic again
remove_clauses(Mod:F/N, Mod:Head) :-
	!, functor(Head, F, N),
	Mod:abolish(F/N),
	Mod:retractall(Head).
remove_clauses(F/N, Head) :-
	functor(Head, F, N),
	abolish(F/N),
	retractall(Head).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% count the number of clauses of a predicate
%% only works when the predicate only consists of facts

%% count_clauses/2
%% prolog variant specific


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% random numbers, goals 

:- use_module(library(lists), [nth0/3]).

%% random/3

%% random_goal/1
%% note that this predicate only succeeds (at most) once
random_goal(Goal) :-
	bagof(Goal, Goal, Goals),
	length(Goals, Max),
	random(0, Max, K),
	nth0(K, Goals, Goal).

%% random_member/2
%% a faster variant of random_goal(member(X,Xs))
%% note that this predicate only succeeds (at most) once
random_member(Y, Xs) :-
	( var(Y) ->
	    Ys = Xs
	;
	    bagof(Y, member(Y, Xs), Ys)
	),
	length(Ys, Max),
	random(0, Max, K),
	nth0(K, Ys, Y).
	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% counting all solutions

%% countall/2
%% prolog variant specific


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% error messages

error(instantiation, Goal, Nr) :-
	format(user_error, 'Happened in subterm ~w of goal: ~w\n', [Nr, Goal]),
	throw(error(instantiation_error, instantiation_error(Goal, Nr))).



