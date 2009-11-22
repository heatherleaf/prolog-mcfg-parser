
%% INCOMPLETE, do not use!

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Incremental bottom-up chart parser for LCFRS as described in
%%     - Ljunglöf 2004, sec 4.6 "Incremental PMCFG parsing"
%%
%% Time-stamp: "2005-09-13, 10:55"
%%
%% MCFG rule formats for this parser:
%%
%%   eps_rule(Lbl, Lins, Rule)
%%   tok_rule(Word, Lbl, Symbols, Lins, Rule)
%%   cat_rule(Cat, PLbl, Nr, Lbl, Symbols, Lins, Rule)
%%
%% optimal performance when Cats and Words are atoms and disjoint, and not []
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(recognize_incremental, [ recognize/3,
				   deduct/3,
				   deduct/2,
				   cleanup/1,
				   initialize/1,
				   final_item/2,
				   final_item/5
				 ]).

:- use_module(utilities, [inbetween/3, remove_clauses/1, update_arg/4]).
:- ensure_loaded(deduct).

:- dynamic final_item/2, final_item/5.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% all different parsing strategies

%% bottom-up, no filtering
recognize(bu, Grammar, Sentence) :-
	deduct(parseBU, Grammar, Sentence),
	create_final_item(Grammar),
	remove_clauses(final_item/5),
	assert((final_item(A,B,C,D,E) :- final(A,B,C,D,E))).

%% bottom-up, top-down filtering
recognize(buF, Grammar, Sentence) :-
	deduct(parseBU, Grammar, Sentence),
	create_final_item(Grammar),
	deduct(filterTD, Grammar).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% creating the final starting items

create_final_item(Grammar) :-
	remove_clauses(final_item/2),
	sentencelength(N),
	Found = [_lbl = 0-N],
	\+ (
	     Grammar:start_mcfg(Start),
	     passive(Start, Found),
	     assert(final_item(Start, Found)),
	     fail
	   ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% item forms

:- dynamic active/8, final/5, passive/2.
:- dynamic sentencelength/1, scan/3.

parseBU:item_form(active(_nextsym, _rule, _rangerec, _lbl, _range, _symbols, _linrec, _rangerecs)).
parseBU:item_form(final(_cat, _rangerec, _cats, _rangerecs, _fun)).
parseBU:item_form(passive(_cat, _rangerec)).

parseBU:subsuming_item(Item) :- Item.

%% When using unification:
%% subsuming_item(Item) :-
%% 	similar_item(Item, Similar),
%% 	call(Similar),
%% 	subsumes_chk(Similar, Item).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% incremental parsing algorithm
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% combine
parseInc:inference(_Grammar,
		   (
		     active(Hash, Rule, Found, Lbl, I, J, [cat(PCat,PLbl,PNr) | Symbols], Lins, Dtrs),
		     passive(Hash, PCat, PFound, PLbl, J, K)
		   ),
		   (
		     arg(PNr, Dtrs, PFound),
		     update_arg(PNr, Dtrs, [r(PLbl,J,K) | PFound], NewDtrs),
		     nexthash(Symbols, K, Next)
		   ),
		   active(Next, Rule, Found, Lbl, I, K, Symbols, Lins, NewDtrs)
		  ).

%% initial scan
parseInc:axiom(Grammar,
	      (
		scan(0, J, Word),
		Grammar:tok_rule(Word, Lbl, Symbols, Lins, Rule),
		%% Rule :--> [Lbl = [tok(Word) | Symbols] | Lins],
		empty_daughters(Rule, Dtrs),
		nexthash(Symbols, J, Next)
	      ), 
	      active(Next, Rule, [], Lbl, 0, J, Symbols, Lins, Dtrs)
	     ).


%% scan
parseInc:axiom(_Grammar,
	       (
		 scan(I, J, Word),
		 I > 0,
		 hash_tok(Word, J, Hash),
		 active(Hash, Rule, Found, Lbl, J, K, [tok(Word) | Symbols], Lins, Dtrs),
		 nexthash(Symbols, K, Next)
	       ),
	       active(Next, Rule, Found, Lbl, I, K, Symbols, Lins, Dtrs)
	      ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% indexing

nexthash([], _, []).
nexthash([tok(Word) | _], K, Hash) :-
	hash_tok(Word, K, Hash).
nexthash([cat(Cat,Lbl,_) | _], K, Hash) :-
	hash_cat(Cat, Lbl, K, Hash).

hash_tok(W, K, H) :- hash_term(tok(W,K), H).
hash_tok(C, L, K, H) :- hash_term(cat(C,L,K), H).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% helper predicates:

empty_daughters(rule(_, _, RHS), Dtrs) :-
	functor(RHS, _, N),
	functor(Dtrs, d, N).

projection(Rec, Lbl, Val) :- member(Lbl=Val, Rec), !.

nextsym([], []).
nextsym([Sym | _], Next) :- arg(1, Sym, Next).
