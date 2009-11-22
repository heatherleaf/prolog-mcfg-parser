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
%% Utilities for syntax trees
%%
%% by Peter Ljunglöf, Sep 2005
%% (Time-stamp: "2006-05-03, 22:55")
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A profile is a representation of a function of N arguments
%% i.e. a tree with "holes" where argument trees can be put
%% arguments can also be unified and restricted
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A *tree* is
%%     - a compund term = a functor applied to a number of trees
%%
%% A *profile* is one of (assuming the arguments Dtr1,...,DtrN)
%%     - '?', corresponds to an unknown tree (anonymous prolog variable)
%%     - integer K, corresponds to the Kth daughter tree DtrK
%%     - P=Q, corresponds to unifying the trees
%%     - a list of profiles
%%     - a compund term = a functor applied to a number of profiles
%% One exception is that if the whole profile is an atom Fun,
%% then it is a shorthand for the profile Fun(1,...,N),
%% i.e. it corresponds to the tree Fun(Dtr1,...,DtrN)
%%
%% Note that a tree is a profile
%% Also note that prolog variables are NOT allowed in profiles
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A chart is a set of items, on the form:
%%     Module:final_item(Cat, Found)
%%     Module:final_item(Cat, Found, f(Cat,...), f(Found,...), Profile)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exported predicates:
%%
%% extract_tree(?Tree)
%% extract_tree(+Module, ?Tree)
%%     - extracts a tree from a chart (default module = user)
%%
%% profile_tree(+Profile, +List of Trees, ?Tree)
%%     - applies a profile to its arguments
%%
%% generate(+Depth, ?Tree)
%% generandom(+Depth, ?Tree)
%%     - equivalent to calling generate/3 (resp generandom/3) with Grammar=user
%%
%% generate(+Grammar, +Depth, ?Tree)
%%     - generate (by backtracking) all possible trees down to a given depth
%%     - Grammar is the module where the grammar resides
%%     - Depth is the maximum search depth
%%
%% generandom(+Grammar, +Depth, ?Tree)
%%     - generate one random tree
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(tree, [ extract_tree/1,
		  extract_tree/2,
		  profile_tree/3,
		  generate/2,
		  generate/3,
		  generandom/2,
		  generandom/3
		]).

:- use_module(utilities, [error/3, random_goal/1, repeat/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% extract_tree/1

extract_tree(Tree) :-
	extract_tree(user, Tree).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% extract_tree/2

extract_tree(Module, Tree) :-
	Module:final_item(Cat, Found),
	extract_tree(Module, Cat, Found, Tree).

extract_tree(Module, Cat, Found, Tree) :-
	Module:final_item(Cat, Found, Cats, Founds, Profile),
	functor(Cats, _, M),
	functor(Trees, ts, M),
	extract_trees(Module, Cats, Founds, Trees, 1, M),
	profile_tree(Profile, Trees, Tree).

extract_trees(Module, Cats, Founds, Trees, N, Max) :-
	( N > Max -> true
	;
	    arg(N, Cats, Cat),
	    arg(N, Founds, Found),
	    arg(N, Trees, Tree),
	    N1 is N+1,
	    extract_tree(Module, Cat, Found, Tree),
	    extract_trees(Module, Cats, Founds, Trees, N1, Max)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generate/2-3
%% generate (by backtracking) all possible trees
%%
%% generandom/2-3
%% generate one random tree

generate(MaxDepth, Tree) :-
	generate(user, MaxDepth, Tree).

generate(Lang, MaxDepth, Tree) :-
	Lang : startsymbol(Cat),
	generate_tree(true, Lang, Cat, Tree, MaxDepth).

generandom(MaxDepth, Tree) :-
	generandom(user, MaxDepth, Tree).

generandom(Lang, MaxDepth, Tree) :-
	random_goal(Lang : startsymbol(Cat)),
	generate_tree(random, Lang, Cat, Tree, MaxDepth).

generate_tree(Flag, Lang, Cat, Tree, Depth) :-
	Depth > 0,
	( Flag == random ->
	    random_goal(Lang : rule(Profile, Cat, Dtrs, _Lin))
	;
	    Lang : rule(Profile, Cat, Dtrs, _Lin)
	),
	functor(Dtrs, _, N),
	functor(Trees, ts, N),
	Depth1 is Depth-1,
	generate_trees(Flag, Lang, Dtrs, Trees, Depth1, 1, N),
	profile_tree(Profile, Trees, Tree).

generate_trees(Flag, Lang, Cats, Trees, Depth, N, Max) :-
	( N > Max -> true
	;
	    arg(N, Cats, Cat0),
	    ( Cat0 = Tree:Cat -> true
	    ; Cat = Cat0 ),
	    arg(N, Trees, Tree),
	    generate_tree(Flag, Lang, Cat, Tree, Depth),
	    N1 is N+1,
	    generate_trees(Flag, Lang, Cats, Trees, Depth, N1, Max)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% profile_tree/3

profile_tree(Fun, Trees, Tree) :-
	atom(Fun),
	!, ( functor(Trees, Fun, _) -> Tree = Trees
	   ;
	       Trees =.. [_ | Dtrs],
	       Tree =.. [Fun | Dtrs]
	   ).
profile_tree(Profile, Trees, Tree) :-
	apply_prof(Profile, Trees, Tree).

apply_prof(X, Fs, T) :-
	var(X), !, error(instantiation, apply_prof(X,Fs,T), 1).
apply_prof(?, _, _Meta) :-
	!.
apply_prof(P1 = P2, Trees, Tree) :-
	!, apply_prof(P1, Trees, Tree),
	apply_prof(P2, Trees, Tree).
apply_prof([], _, _) :-
	!, fail.
apply_prof([P|Ps], Trees, Tree) :-
	!, member(Prof, [P|Ps]),
	apply_prof(Prof, Trees, Tree).
apply_prof(Nr, Trees, Tree) :-
	integer(Nr),
	!, arg(Nr, Trees, Tree).
apply_prof(Profile, Trees, Tree) :-
	functor(Profile, Fun, M),
	functor(Tree, Fun, M),
	apply_prof(Profile, Trees, Tree, 1, M).

apply_prof(Profiles, Trees, Tree, N, Max) :-
	( N > Max -> true
	;
	    arg(N, Profiles, Profile),
	    arg(N, Tree, DtrTree),
	    N1 is N+1,
	    apply_prof(Profile, Trees, DtrTree),
	    apply_prof(Profiles, Trees, Tree, N1, Max)
	).

