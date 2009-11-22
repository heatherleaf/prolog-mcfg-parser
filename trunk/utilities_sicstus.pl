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
%% Sicstus specific utilities
%% Time-stamp: "2006-01-12, 13:45"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- meta_predicate
	countall(:, ?),
	count_clauses(:, ?).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% all integers between From and To, on backtracking

inbetween(From, To, Result) :-
	From =< To,
	( Result = From
	; From1 is From+1, inbetween(From1, To, Result)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% updating an argument of a compound term

update_arg(1, T, X, TT) :- update_arg1(T, X, TT).
update_arg(2, T, X, TT) :- update_arg2(T, X, TT).
update_arg(3, T, X, TT) :- update_arg3(T, X, TT).
update_arg(4, T, X, TT) :- update_arg4(T, X, TT).
update_arg(5, T, X, TT) :- update_arg5(T, X, TT).
update_arg(6, T, X, TT) :- update_arg6(T, X, TT).
update_arg(7, T, X, TT) :- update_arg7(T, X, TT).
update_arg(8, T, X, TT) :- update_arg8(T, X, TT).
update_arg(9, T, X, TT) :- update_arg9(T, X, TT).

update_arg1(d(_), A, d(A)).
update_arg1(d(_,B), A, d(A,B)).
update_arg1(d(_,B,C), A, d(A,B,C)).
update_arg1(d(_,B,C,D), A, d(A,B,C,D)).
update_arg1(d(_,B,C,D,E), A, d(A,B,C,D,E)).
update_arg1(d(_,B,C,D,E,F), A, d(A,B,C,D,E,F)).
update_arg1(d(_,B,C,D,E,F,G), A, d(A,B,C,D,E,F,G)).
update_arg1(d(_,B,C,D,E,F,G,H), A, d(A,B,C,D,E,F,G,H)).
update_arg1(d(_,B,C,D,E,F,G,H,I), A, d(A,B,C,D,E,F,G,H,I)).

update_arg2(d(A,_), B, d(A,B)).
update_arg2(d(A,_,C), B, d(A,B,C)).
update_arg2(d(A,_,C,D), B, d(A,B,C,D)).
update_arg2(d(A,_,C,D,E), B, d(A,B,C,D,E)).
update_arg2(d(A,_,C,D,E,F), B, d(A,B,C,D,E,F)).
update_arg2(d(A,_,C,D,E,F,G), B, d(A,B,C,D,E,F,G)).
update_arg2(d(A,_,C,D,E,F,G,H), B, d(A,B,C,D,E,F,G,H)).
update_arg2(d(A,_,C,D,E,F,G,H,I), B, d(A,B,C,D,E,F,G,H,I)).

update_arg3(d(A,B,_), C, d(A,B,C)).
update_arg3(d(A,B,_,D), C, d(A,B,C,D)).
update_arg3(d(A,B,_,D,E), C, d(A,B,C,D,E)).
update_arg3(d(A,B,_,D,E,F), C, d(A,B,C,D,E,F)).
update_arg3(d(A,B,_,D,E,F,G), C, d(A,B,C,D,E,F,G)).
update_arg3(d(A,B,_,D,E,F,G,H), C, d(A,B,C,D,E,F,G,H)).
update_arg3(d(A,B,_,D,E,F,G,H,I), C, d(A,B,C,D,E,F,G,H,I)).

update_arg4(d(A,B,C,_), D, d(A,B,C,D)).
update_arg4(d(A,B,C,_,E), D, d(A,B,C,D,E)).
update_arg4(d(A,B,C,_,E,F), D, d(A,B,C,D,E,F)).
update_arg4(d(A,B,C,_,E,F,G), D, d(A,B,C,D,E,F,G)).
update_arg4(d(A,B,C,_,E,F,G,H), D, d(A,B,C,D,E,F,G,H)).
update_arg4(d(A,B,C,_,E,F,G,H,I), D, d(A,B,C,D,E,F,G,H,I)).

update_arg5(d(A,B,C,D,_), E, d(A,B,C,D,E)).
update_arg5(d(A,B,C,D,_,F), E, d(A,B,C,D,E,F)).
update_arg5(d(A,B,C,D,_,F,G), E, d(A,B,C,D,E,F,G)).
update_arg5(d(A,B,C,D,_,F,G,H), E, d(A,B,C,D,E,F,G,H)).
update_arg5(d(A,B,C,D,_,F,G,H,I), E, d(A,B,C,D,E,F,G,H,I)).

update_arg6(d(A,B,C,D,E,_), F, d(A,B,C,D,E,F)).
update_arg6(d(A,B,C,D,E,_,G), F, d(A,B,C,D,E,F,G)).
update_arg6(d(A,B,C,D,E,_,G,H), F, d(A,B,C,D,E,F,G,H)).
update_arg6(d(A,B,C,D,E,_,G,H,I), F, d(A,B,C,D,E,F,G,H,I)).

update_arg7(d(A,B,C,D,E,F,_), G, d(A,B,C,D,E,F,G)).
update_arg7(d(A,B,C,D,E,F,_,H), G, d(A,B,C,D,E,F,G,H)).
update_arg7(d(A,B,C,D,E,F,_,H,I), G, d(A,B,C,D,E,F,G,H,I)).

update_arg8(d(A,B,C,D,E,F,G,_), H, d(A,B,C,D,E,F,G,H)).
update_arg8(d(A,B,C,D,E,F,G,_,I), H, d(A,B,C,D,E,F,G,H,I)).

update_arg9(d(A,B,C,D,E,F,G,H,_), I, d(A,B,C,D,E,F,G,H,I)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% count the number of clauses of a predicate
%% only works when the predicate only consists of facts

count_clauses(Head, Nr) :-
	countall(Head, Nr).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% random numbers

:- use_module(library(random), [setrand/1, random/3]).
:- use_module(library(system), [datime/1]).

randomize :-
        datime(datime(Yr,Mn,Dy,Hr,Mi,Sd)),
        X is Yr*Mn+1, Y is Dy*Hr+1, Z is Mi*Sd+1,
        setrand(rand(X,Y,Z)).

:- randomize.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% counting all solutions

countall(Goal, Times) :-
	bb_put(succeeds_n_times, 0),
	\+ (
	     Goal,
	     bb_get(succeeds_n_times, N),
	     N1 is N+1,
	     bb_put(succeeds_n_times, N1),
	     fail
	   ),
	bb_get(succeeds_n_times, Times).


