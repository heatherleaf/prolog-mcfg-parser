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
%% Active bottom-up chart parser for MCFG as described in
%%     - Ljunglöf 2004, sec 4.4 "Active parsing of PMCFG"
%%     - Burden 2005, sec 3.4
%%
%% Time-stamp: "2006-01-12, 13:32"
%%
%% MCFG rule formats for this parser:
%%
%%    eps_rule(Lbl, Lins, Rule)
%%    tok_rule(Word, Lbl, Symbols, Lins, Rule)
%%    cat_rule(Cat, PLbl, Nr, Lbl, Symbols, Lins, Rule)
%%
%% performance is optimal when Cats and Words are atoms and disjoint, and not []
%%
%% the grammar should contain a predicate defining the starting category(-ies):
%%
%%    startsymbol(Cat)
%%
%% the recognition process results in a parse chart with the following predicates:
%%
%%    final_item(Category, Positions)
%%        - the starting Category is found spanning Positions in the input
%%
%%    final_item(Category, Positions, DaughterCategories, DaughterPositions, Name)
%%        - Category spanning Positions is found with
%%          DaughterCategories spanning DaughterPositions
%%        - Name is the name of the MCFG rule
%%          (for standard CFG, this would be Category)
%%        - Name can also be a Profile (see tree.pl for more information on profiles)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exported predicates:
%%
%%    recognize(+Strategy, +Sentence)
%%    recognize(+Strategy, +Grammar, +Sentence)
%%        - builds a parse chart for the Sentence using the given parsing Strategy
%%          (see 25 lines below for a list of possible strategies)
%%        - the MCFG grammar resides in the Prolog module Grammar
%%        - default grammar module = 'user'
%%
%%    deduct/2, deduct/3, cleanup/1, initialize/1
%%        - these are described in deduct.pl
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(recognize_active, [ recognize/2,
			      recognize/3,
			      deduct/3,
			      deduct/2,
			      cleanup/1,
			      initialize/1,
			      final_item/2,
			      final_item/5
			    ]).

:- use_module(utilities, [inbetween/3, remove_clauses/1]).
:- use_module(library(lists), [member/2]).
:- ensure_loaded(deduct).

:- dynamic final_item/2, final_item/5.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% all different parsing strategies

recognize(Strategy, Sentence) :-
	recognize(Strategy, user, Sentence).

%% bottom-up, no filtering
recognize(bu, Grammar, Sentence) :-
	deduct(recognizeBU, Grammar, Sentence),
	remove_clauses(final_item/2),
	remove_clauses(final_item/5),
	sentencelength(N), Found = [_ = 0-N],
	assert((final_item(Start, Found) :- Grammar:startsymbol(Start), passive(Start, Found))),
	assert((final_item(A,B,C,D,E) :- final(A,B,C,D,E))).

%% bottom-up, top-down filtering
recognize(buF, Grammar, Sentence) :-
	deduct(recognizeBU, Grammar, Sentence),
	deduct(filterTD, Grammar).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bottom-up parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% item forms

:- dynamic active/8, final/5, passive/2.
:- dynamic sentencelength/1, scan/3.

recognizeBU:item_form(active(_nextsym, _rule, _rangerec, _lbl, _range, _symbols, _linrec, _rangerecs)).
recognizeBU:item_form(final(_cat, _rangerec, _cats, _rangerecs, _fun)).
recognizeBU:item_form(passive(_cat, _rangerec)).

recognizeBU:subsuming_item(Item) :- Item.

%% When using unification:
%% subsuming_item(Item) :-
%% 	similar_item(Item, Similar),
%% 	call(Similar),
%% 	subsumes_chk(Similar, Item).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% axioms

%%                                                  
%% initial ---------------------------------------- Rule := r = w{i-j} \alpha, \phi
%% scan     [Rule; r = i-j . \alpha, \phi; {}..{}]
%%
recognizeBU:axiom(Grammar,
		  (
		    scan(I, J, Word),
		    Grammar:tok_rule(Word, Lbl, Symbols, Lins, Rule),
		    %% Rule ::= [Lbl = [tok(Word) | Symbols] | Lins],
		    empty_daughters(Rule, Dtrs),
		    nextsym(Symbols, Next)
		  ), 
		  active(Next, Rule, [], Lbl, I-J, Symbols, Lins, Dtrs)
		 ).

%%                                            
%% epsilon ---------------------------------- Rule := r = \epsilon, \phi
%% rules    [Rule; r = --- . , \phi; {}..{}]
%%
recognizeBU:axiom(Grammar,
		  (
		    Grammar:eps_rule(Lbl, Lins, Rule),
		    %% Rule ::= [Lbl = [] | Lins],
		    empty_daughters(Rule, Dtrs),
		    nextsym([], Next)
		  ),
		  active(Next, Rule, [], Lbl, ---, [], Lins, Dtrs)
		 ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% inference rules

%%           [Rule; \Gamma, r = \rho . , r' = \alpha, \phi; \GammaVec]
%% complete --------------------------------------------------------------
%%           [Rule; \Gamma, r = \rho, r' = --- . \alpha, \phi; \GammaVec]
%%
recognizeBU:inference(_Grammar,
		      active(_, Rule, Found, FLbl, FRng, [], [Lbl=Symbols | Lins], Dtrs),
		      (
			nextsym(Symbols, Next)
		      ),
		      active(Next, Rule, [FLbl=FRng | Found], Lbl, ---, Symbols, Lins, Dtrs)
		     ).

%%       [Rule; \Gamma, r = \rho . w{j-k} \alpha, \phi; \GammaVec]
%% scan ----------------------------------------------------------- \rho' = concat(\rho, j-k)
%%       [Rule; \Gamma, r = \rho' . \alpha, \phi; \GammaVec]
%%
recognizeBU:inference(_Grammar,
		      active(Word, Rule, Found, Lbl, Rng, [tok(_Word) | Symbols], Lins, Dtrs),
		      (
			concat_range(Rng, J-K, NewRng),
			scan(J, K, Word),
			nextsym(Symbols, Next)
		      ),
		      active(Next, Rule, Found, Lbl, NewRng, Symbols, Lins, Dtrs)
		     ).

%%        [Rule; \Gamma, r = \rho . ; \GammaVec]
%% final ----------------------------------------
%%        [Rule := \Gamma, r = \rho ; \GammaVec]
%%
recognizeBU:inference(_Grammar,
		      active([], rule(Fun, Cat, Cats), Found, Lbl, Rng, [], [], Dtrs),
		      true,
		      final(Cat, [Lbl=Rng | Found], Cats, Dtrs, Fun)
		     ).

%%          [A -> f[..] := \Gamma ; ..]
%% passive -----------------------------
%%          [A; \Gamma]
%%
recognizeBU:inference(_Grammar,
		      final(Cat, Found, _, _, _),
		      true,
		      passive(Cat, Found)
		     ).

%%           [B_i; \Gamma_i]                                    Rule := B_i.r'
%% predict  --------------------------------------------------- Rule = A -> f[..B_i..]
%% bottomup  [Rule; r = \rho . \alpha, \phi; {}..\Gamma_i..{}]  \rho = \Gamma_i.r'
%%
recognizeBU:inference(Grammar,
		      passive(Cat, Found),
		      (
			Grammar:cat_rule(Cat, PLbl, Nr, Lbl, Symbols, Lins, Rule),
			%% Rule ::= [Lbl = [arg(Cat,Nr,PLbl) | Symbols] | Lins],
			projection(Found, PLbl, Rng),
			empty_daughters(Rule, Dtrs),
			arg(Nr, Dtrs, Found),
			nextsym(Symbols, Next)
		      ),
		      active(Next, Rule, [], Lbl, Rng, Symbols, Lins, Dtrs)
		     ).

%%          [Rule; \Gamma, r = \rho . B_i.r' \alpha, \phi; ..\Gamma_i..]
%%          [B_i; \Gamma']                                                \rho' = concat(\rho, \Gamma'.r')
%% combine -------------------------------------------------------------- \Gamma_i = {} or \Gamma'
%%          [Rule; \Gamma, r = \rho' . \alpha, \phi; ..\Gamma'..]
%%
recognizeBU:inference(_Grammar,
		      (
			active(PCat, Rule, Found, Lbl, Rng, [arg(_PCat,PNr,PLbl) | Symbols], Lins, Dtrs),
			passive(PCat, PFound)
		      ),
		      (
			projection(PFound, PLbl, PRng),
			concat_range(Rng, PRng, NewRng),
			arg(PNr, Dtrs, PFound),
			nextsym(Symbols, Next)
		      ),
		      active(Next, Rule, Found, Lbl, NewRng, Symbols, Lins, Dtrs)
		     ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ranges: (J-K) | (---) = epsilon range

%% concat(---, ---)  = ---
%% concat(---, j-k)  = j-k
%% concat(i-j, ---)  = i-j
%% concat(i-j, j'-k) = i-k  iff  j = j'

concat_range(---, Rng, Rng).
concat_range(I-J, JK, I-K) :-
	concat_range2(JK, J, K).

concat_range2(---, K, K).
concat_range2(J-K, J, K).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% helper predicates:

empty_daughters(rule(_, _, RHS), Dtrs) :-
	functor(RHS, _, N),
	functor(Dtrs, d, N).

projection(Rec, Lbl, Val) :- member(Lbl=Val, Rec), !.

nextsym([], []).
nextsym([Sym | _], Next) :- arg(1, Sym, Next).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% top-down filtering of the final chart
%%
%% each item final/5 which can be reached from the startsymbol
%% (i.e. from a final_item/2)
%% is copied to final_item/5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

filterTD:item_form(final_item(_cat, _rangerec, _cats, _rangerecs, _fun)).
filterTD:item_form(final_item(_cat, _rangerec)).

filterTD:subsuming_item(Item) :- Item.


%%
%% 	------------------ [S; s = 0-N]
%% 	 [* S; s = 0-N *]
%%
filterTD:axiom(Grammar,
	       (
		 sentencelength(N),
		 Found = [_lbl = 0-N],
		 Grammar:startsymbol(Start),
		 passive(Start, Found)
	       ),
	       final_item(Start, Found)
	      ).

%% 	 [* A; \Gamma *]
%% 	---------------------------------- [Rule := \Gamma; \GammaVec]
%% 	 [* Rule := \Gamma; \GammaVec *]    Rule = A -> f[..]
%%
filterTD:inference(_,
		   final_item(Cat, Found),
		   final(Cat, Found, Cats, Dtrs, Fun),
		   final_item(Cat, Found, Cats, Dtrs, Fun)
		  ).

%% 	 [* A -> f[..B_i..] := ..; ..\Gamma_i.. *]
%% 	------------------------------------------- [Rule_i := \Gamma_i; \GammaVec]
%% 	 [* Rule_i := \Gamma_i ; \GammaVec *]        Rule_i = B_i -> g[..]
%%
filterTD:inference(_,
		   final_item(_, _, DtrCats, DtrFounds, _),
		   (
		     functor(DtrCats, _, Arity),
		     inbetween(1, Arity, N),
		     arg(N, DtrCats, Cat),
		     arg(N, DtrFounds, Found),
		     final(Cat, Found, Cats, Dtrs, Fun)
		   ),
		   final_item(Cat, Found, Cats, Dtrs, Fun)
		  ).


