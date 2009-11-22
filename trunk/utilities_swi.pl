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
%% SWI specific utilities
%% Time-stamp: "2006-01-12, 13:45"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- meta_predicate
	countall(:, ?),
	count_clauses(:, ?).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% all integers between From and To, on backtracking
%% this predicate already exists in SWI, with the name between/3

inbetween(From, To, Result) :-
	between(From, To, Result).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% updating an argument of a compound term

update_arg(Nr, Dtrs, Dtr, Dtrs) :-
	setarg(Nr, Dtrs, Dtr).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% count the number of clauses of a predicate
%% only works when the predicate only consists of facts

count_clauses(Head, Nr) :-
	predicate_property(Head, number_of_clauses(Nr)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% random numbers

random(Min, Max, K) :-
	K is Min + random(Max-Min).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% counting all solutions

countall(Goal, Times) :-
	flag(succeeds_n_times, Old, 0),
	\+ (
	     Goal,
	     flag(succeeds_n_times, N, N+1),
	     fail
	   ),
	flag(succeeds_n_times, Times, Old).
