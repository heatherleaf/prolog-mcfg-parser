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
%% Definitions for MCFG and GFC (an extension of MCFG)
%% By Peter Ljunglöf (Time-stamp: "2006-04-18, 10:05")
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% * MCFG rule format (using labelled records instead of tuples)
%%
%% Language : rule(Profile, Cat, f(Cat1,...,CatN), [Lin1,...,LinM])
%%
%%     Language: Prolog module
%%     Profile:  profile (see tree.pl or forest.pl)
%%     Lin:      Lbl=[Sym,...]
%%     Sym:      arg(Cat, ArgNr, Lbl)
%% 	         tok(Token)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% * GFC rule format (= MCFG rules, with different linearizations)
%%
%% Language : rule(Profile, Cat, f(Cat1,...,CatN), LinTerm)
%%
%%     Language: Prolog module
%%     Profile:  profile (see tree.pl or forest.pl)
%%     LinTerm:  arg(Cat, ArgNr, Path)          (where Path = [Lbl or Pat,...])
%%               Con ^ [LinTerm,...]
%%               rec([Lbl=LinTerm,...])
%%               tbl([Pat=LinTerm,...])
%%               variants([LinTerm,...])
%%               LinTerm + LinTerm
%%               tok(Word)
%%               empty
%%               LinTerm * Lbl                 (record projection)
%%               LinTerm / LinTerm             (table selection)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exported predicates:
%%
%%    language(?Lang)
%%        - Lang is a Prolog module containing a MCFG grammar
%%
%%    expand_variants(+LinTerm, ?LinTerm without variants)
%%
%%    linearize(+Tree, +Lbl, ?Symbols)
%%    linearize(+Lang, +Tree, +Lbl, ?Symbols)
%%        - Symbols is a linearization of the Tree at label Lbl for grammar module Lang
%%        - Default grammar module = 'user'
%%
%%    linearize_gfc(+Tree, +Path, ?Term)
%%    linearize_gfc(+Lang, +Tree, +Path, ?Term)
%%        - Term is a linearization of the Tree in path Path for the GFC grammar Lang
%%        - Path is a list of labels (or selection terms)
%%        - Default grammar module = 'user'
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(mcfg, [ language/1,
		  expand_variants/2,
		  linearize/3,
		  linearize/4,
		  linearize_gfc/3,
		  linearize_gfc/4,
		  linearandom/3,
		  linearandom/4,
		  linearandom_gfc/3,
		  linearandom_gfc/4
		]).

:- use_module(library(lists), [member/2, append/3, nth0/3]).
:- use_module(utilities, [random_member/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% language/1

language(Module) :-
	current_module(Module),
	Module:current_predicate(rule/4),
	\+ member(Module, [mcfg, prolog, license, toplevel_variables, lists, link_xpce, pce_swi_hooks]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% expand_variants/2
%% variants are encoded as several consecutive linearization rows
%% with the same label

expand_variants([], []) :- !.
expand_variants(Lins, [Lin | SelectedLins]) :-
	Lins = [Lbl=_ | _],
	append(LblLins, RestLins, Lins),
	RestLins \= [Lbl=_ | _],
	!, member(Lin, LblLins),
	expand_variants(RestLins, SelectedLins).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% linearize/3-4
%% linearizing parse trees to linearization terms
%% only works when the profiles are only atoms
%% generate all possible linearizations (by backtracking)
%%
%% linearandom/3-4
%% generating one random linearization

linearize(Tree, Lbl, Lin) :-
	linearize(user, Tree, Lbl, Lin).

linearize(Language, Tree, Lbl, Lin) :-
	linearize_term(nondet, Language, Tree, _Cat, Lins),
	member(Lbl = Lin, Lins).

linearandom(Tree, Lbl, Lin) :-
	linearandom(user, Tree, Lbl, Lin).

linearandom(Language, Tree, Lbl, Lin) :-
	linearize_term(random, Language, Tree, _Cat, Lins),
	random_member(Lbl = Lin, Lins).

linearize_term(Random, Language, Tree, Cat, Term) :-
	functor(Tree, Fun, N),
	functor(Args, t, N),
	Language : rule(Fun, Cat, Cats, Term0),
	linearize_many(Random, Language, Tree, Cats, Args, 1, N),
	apply_lins(Term0, Random, Args, Term).

linearize_many(Random, Language, Tree, Cats, Terms, N, Max) :-
	( N > Max -> true
	;
	    arg(N, Tree, DtrTree),
	    arg(N, Cats, Cat),
	    arg(N, Terms, Term),
	    linearize_term(Random, Language, DtrTree, Cat, Term),
	    N1 is N+1,
	    linearize_many(Random, Language, Tree, Cats, Terms, N1, Max)
	).

apply_lins([], _, _, []).
apply_lins([Lbl=Lin0 | Lins0], Random, Args, [Lbl=Lin | Lins]) :-
	apply_lin(Lin0, Random, Args, Lin, []),
	apply_lins(Lins0, Random, Args, Lins).

apply_lin([], _, _) --> [].
apply_lin([arg(_,Nr,Lbl) | Lin], Random, Args) -->
	{ arg(Nr, Args, DtrLins),
	  ( Random == random ->
	      random_member(Lbl = DtrLin, DtrLins)
	  ;
	      member(Lbl = DtrLin, DtrLins)
	  ) },
	DtrLin,
	apply_lin(Lin, Random, Args).
apply_lin([tok(Word) | Lin], Random, Args) -->
	[Word],
	apply_lin(Lin, Random, Args).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% linearize_gfc/3-4
%% linearizing parse trees to GFC linearization terms
%% generate all possible linearizations (by backtracking)
%%
%% linearandom_gfc/3-4
%% generating one random linearization

linearize_gfc(Tree, Path, Term) :-
	linearize_gfc(user, Tree, Path, Term).

linearize_gfc(Language, Tree, Path, Term) :-
	linearize_gfc_term(nondet, Language, Tree, _Cat, Term0),
	follow_path(Path, Term0, Term1),
	flatten_term(Term1, Term).

linearandom_gfc(Tree, Path, Term) :-
	linearandom_gfc(user, Tree, Path, Term).

linearandom_gfc(Language, Tree, Path, Term) :-
	linearize_gfc_term(random, Language, Tree, _Cat, Term0),
	follow_path(Path, Term0, Term1),
	flatten_term(Term1, Term).


linearize_gfc_term(Random, Language, Tree, Cat, Term) :-
	( var(Tree) ->
	    Term = Tree
	;
	    functor(Tree, Fun, N),
	    functor(Args, t, N),
	    Language : rule(Fun, Cat, Cats, Term0),
	    linearize_gfc_many(Random, Language, Tree, Cats, Args, 1, N),
	    apply_linterm(Term0, Random, Args, Term)
	).

linearize_gfc_many(Random, Language, Tree, Cats, Terms, N, Max) :-
	( N > Max -> true
	;
	    arg(N, Tree, DtrTree),
	    arg(N, Cats, Cat0),
	    ( Cat0 = DtrTree:Cat -> true
	    ; Cat = Cat0 ),
	    arg(N, Terms, Term),
	    linearize_gfc_term(Random, Language, DtrTree, Cat, Term),
	    N1 is N+1,
	    linearize_gfc_many(Random, Language, Tree, Cats, Terms, N1, Max)
	).

%% flattening a gfc linearization term to "readable" format

flatten_term(X, Y) :- var(X), !, X=Y.
flatten_term(rec(Rec0), Rec) :- !, flatten_rec(Rec0, Rec).
flatten_term(tbl(Tbl0), Tbl) :- !, flatten_tbl(Tbl0, Tbl).
flatten_term(Term, Term) :- Term = _^_, !.
flatten_term(StrTerm, String) :- flatten_string(StrTerm, String, []).

flatten_rec([], []).
flatten_rec([Lbl=Term0 | Rec0], [Lbl=Term | Rec]) :-
	flatten_term(Term0, Term),
	flatten_rec(Rec0, Rec).

flatten_tbl([], []).
flatten_tbl([Pat0=Term0 | Tbl0], [Pat=Term | Tbl]) :-
	flatten_term(Pat0, Pat),
	flatten_term(Term0, Term),
	flatten_tbl(Tbl0, Tbl).

flatten_string(X) --> {var(X)}, !, [X].
flatten_string(empty) --> [].
flatten_string(T + TT) --> flatten_string(T), flatten_string(TT).
flatten_string(tok(Word)) --> [Word].

%% applying argument linearizations to a linterm

apply_linterm(variants([]), _Random, _Args, variants([])) :- !.
apply_linterm(arg(_Cat,Nr,Path), _Random, Args, Term) :-
	arg(Nr, Args, Arg),
	follow_path(Path, Arg, Term).
apply_linterm(Con ^ Terms0, Random, Args, Con ^ Terms) :-
	apply_linterms(Terms0, Random, Args, Terms).
apply_linterm(rec(Rec0), Random, Args, rec(Rec)) :-
	apply_rec(Rec0, Random, Args, Rec).
apply_linterm(tbl(Tbl0), Random, Args, tbl(Tbl)) :-
	apply_tbl(Tbl0, Random, Args, Tbl).
apply_linterm(variants(Terms0), Random, Args, Term) :-
	( Random == random ->
	    random_member(Term0, Terms0)
	;
	    member(Term0, Terms0)
	),
	apply_linterm(Term0, Random, Args, Term).
apply_linterm(T0 + TT0, Random, Args, T + TT) :-
	apply_linterm(T0, Random, Args, T),
	apply_linterm(TT0, Random, Args, TT).
apply_linterm(tok(Word), _Random, _Args, tok(Word)).
apply_linterm(empty, _Random, _Args, empty).
apply_linterm(Term0 * Lbl, Random, Args, Term) :-
	apply_linterm(Term0, Random, Args, Rec),
	select_rec(Rec, Lbl, Term).
apply_linterm(Term0 / Sel0, Random, Args, Term) :-
	apply_linterm(Term0, Random, Args, Tbl),
	apply_linterm(Sel0, Random, Args, Sel),
	select_tbl(Tbl, Sel, Term).

select_tbl(variants([]), _, variants([])) :- !.
select_tbl(_, variants([]), variants([])) :- !.
select_tbl(tbl(Tbl), Sel, Term) :-
	member(Sel=Term, Tbl).

select_rec(variants([]), _, variants([])) :- !.
select_rec(rec(Rec), Lbl, Term) :-
	member(Lbl=Term, Rec).

apply_linterms([], _Random, _Args, []).
apply_linterms([T0|Ts0], Random, Args, [T|Ts]) :-
	apply_linterm(T0, Random, Args, T),
	apply_linterms(Ts0, Random, Args, Ts).

apply_rec([], _Random, _Args, []).
apply_rec([Lbl=Term0 | Rec0], Random, Args, [Lbl=Term | Rec]) :-
	apply_linterm(Term0, Random, Args, Term),
	apply_rec(Rec0, Random, Args, Rec).

apply_tbl([], _Random, _Args, []).
apply_tbl([Pat0=Term0 | Tbl0], Random, Args, [Pat=Term | Tbl]) :-
	apply_linterm(Pat0, Random, Args, Pat),
	apply_linterm(Term0, Random, Args, Term),
	apply_rec(Tbl0, Random, Args, Tbl).

%% following a path in a linterm

follow_path(_, variants([]), variants([])) :- !.
follow_path([], Term, Term).
follow_path([P | Path], Term0, Term) :-
	follow_path2(Term0, P, Path, Term).

follow_path2(rec(Rec), Lbl, Path, Term) :-
	member(Lbl=Term0, Rec),
	!, follow_path(Path, Term0, Term).
follow_path2(tbl(Tbl), Sel, Path, Term) :-
	member(Sel=Term0, Tbl),
	!, follow_path(Path, Term0, Term).


