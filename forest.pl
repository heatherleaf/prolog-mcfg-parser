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
%% Utilities for syntax forests
%%
%% by Peter Ljunglöf, Sep 2005
%% (Time-stamp: "2005-12-09, 14:44")
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A forest is a compact representation of a set of trees
%%
%% A profile is a representation of a function of N arguments
%% i.e. a forest with "holes" where argument forests can be put
%% arguments can also be unified and restricted
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A *tree* is
%%     - a compund term = a functor applied to a number of trees
%%
%% A *forest* is one of
%%     - a list of forests
%%     - a compound term = a functor applied to a number of forests
%%
%% A *profile* is one of (assuming the arguments Dtr1,...,DtrN)
%%     - '?', corresponds to an unknown forest (anonymous prolog variable)
%%     - integer K, corresponds to the Kth daughter forest DtrK
%%     - P=Q, corresponds to unifying the forests
%%            or taking the intersection of the corresponding sets of trees
%%     - a list of profiles
%%     - a compund term = a functor applied to a number of profiles
%% One exception is that if the whole profile is an atom Fun,
%% then it is a shorthand for the profile Fun(1,...,N),
%% i.e. it corresponds to the forest Fun(Dtr1,...,DtrN)
%%
%% Note that a tree is a forest, and a forest is a profile
%% Also note that prolog variables are NOT allowed in profiles
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A chart is a set of items, on the form:
%%     Module:final_item(Cat, Found)
%%     Module:final_item(Cat, Found, f(Cat,...), f(Found,...), Profile)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Defined predicates:
%%
%% extract_forest(?Forest)
%% extract_forest(+Module, ?Forest)
%%     - extracts a forest from a chart (default module = user)
%% 
%% forest_member(+Forest, ?Tree)
%%     - nondeterministically extracts each tree from a forest
%%
%% profile_forest(+Profile, +List of Forests, ?Forest)
%%     - applies a profile to its arguments
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(forest, [ extract_forest/1,
		    extract_forest/2,
		    forest_member/2,
		    profile_forest/3
		  ]).

:- use_module(utilities, [error/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% extract_forest/1

extract_forest(Forest) :-
	extract_forest(user, Forest).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% extract_forest/2

extract_forest(Module, Forest) :-
	bagof(F, extract_forest_1(Module, F), Fs),
	( Fs = [Forest] -> true ; Fs = Forest ).

extract_forest_1(Module, Forest) :-
	Module:final_item(Cat, Found),
	extract_forest(Module, Cat, Found, Forest).

extract_forest(Module, Cat, Found, Forest) :-
	bagof(F, extract_forest_1(Module, Cat, Found, F), Fs),
	( Fs = [Forest] -> true ; Fs = Forest ).

extract_forest_1(Module, Cat, Found, Forest) :-
	Module:final_item(Cat, Found, Cats, Founds, Profile),
	functor(Cats, _, M),
	functor(Forests, fs, M),
	extract_forests(Module, Cats, Founds, Forests, 1, M),
	profile_forest(Profile, Forests, Forest).

extract_forests(Module, Cats, Founds, Forests, N, Max) :-
	( N > Max -> true
	;
	    arg(N, Cats, Cat),
	    arg(N, Founds, Found),
	    arg(N, Forests, Forest),
	    N1 is N+1,
	    extract_forest(Module, Cat, Found, Forest),
	    extract_forests(Module, Cats, Founds, Forests, N1, Max)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% forest_member/2

forest_member(Forest, Tree) :-
	( var(Forest) ->
	    error(instantiation, forest_member(Forest,Tree), 1)
	; Forest == [] ->
	    fail
	; Forest = [_|_] ->
	    member(F, Forest),
	    forest_member(F, Tree)
	;
	    functor(Forest, Fun, M),
	    functor(Tree, Fun, M),
	    forest_member(Forest, Tree, 1, M)
	).

forest_member(Forest, Tree, N, Max) :-
	( N > Max -> true
	;
	    arg(N, Forest, DtrForest),
	    arg(N, Tree, DtrTree),
	    N1 is N+1,
	    forest_member(DtrForest, DtrTree),
	    forest_member(Forest, Tree, N1, Max)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% profile_forest/3

profile_forest(Fun, Forests, Forest) :-
	atom(Fun),
	!,  ( functor(Forests, Fun, _) -> Forest = Forests
	    ;
		Forests =.. [_ | Dtrs],
		Forest =.. [Fun | Dtrs]
	    ).
profile_forest(Profile, Forests, Forest) :-
	apply_prof(Profile, Forests, Forest).

apply_prof(X, Fs, T) :-
	var(X), !, error(instantiation, apply_prof(X,Fs,T), 1).
apply_prof(?, _, _Meta) :-
	!.
apply_prof(P1 = P2, Forests, Forest) :-
	!, apply_prof(P1, Forests, Forest1),
	apply_prof(P2, Forests, Forest2),
	unify_forests(Forest1, Forest2, Forest).
apply_prof([], _, _) :-
	!, fail.
apply_prof([P|Ps], Forests, Forest) :-
	!, bagof(F, Prof^(
			  member(Prof, [P|Ps]),
			  apply_prof(Prof, Forests, F)
			 ), Forest).
apply_prof(Nr, Forests, Forest) :-
	integer(Nr),
	!, arg(Nr, Forests, Forest).
apply_prof(Profile, Forests, Forest) :-
	functor(Profile, Fun, M),
	functor(Forest, Fun, M),
	apply_prof(Profile, Forests, Forest, 1, M).

apply_prof(Profiles, Forests, Forest, N, Max) :-
	( N > Max -> true
	;
	    arg(N, Profiles, Profile),
	    arg(N, Forest, DtrForest),
	    N1 is N+1,
	    apply_prof(Profile, Forests, DtrForest),
	    apply_prof(Profiles, Forests, Forest, N1, Max)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% unify_forests/2
%% inefficient definition

unify_forests(F1, F2, F) :-
	bagof(T, (
		   forest_member(F1, T),
		   forest_member(F2, T)
		 ), F).
