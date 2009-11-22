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
%% Simple implementation of deductive reasoning
%%
%% by Peter Ljunglöf, Oct 2002
%% modified Apr, Nov 2003, Nov 2004, Aug 2005
%% (Time-stamp: "2005-12-09, 14:44")
%%
%% inspired by Shieber, Schabes & Pereria (1995)
%%    "Principles and Implementation of Deductive Parsing"
%%    Journal of Logic Programming, 24(1-2): pp 3-36.
%% but generalized to a general inference engine
%%
%% works with both Sicstus and SWI Prolog
%% provided that the correct version of utilities.pl is chosen
%%
%% this is not a module, it should be included in the parsing module by
%%
%%    :- ensure_loaded(deduct).
%%
%% the following predicates needs to be defined by the calling module Parser:
%%
%%    Parser:item_form(?Item)
%%        - each of the item forms needs to be declared dynamic
%%    Parser:subsuming_item(+Item)
%%        - if not using unification, it is simply Parser:call(Item)
%%        - if using unification, it can be (in the general case)
%%          Parser:item_form(Similar), Parser:call(Similar), subsumes_chk(Similar, Item)
%%    Parser:axiom(+Grammar, ?SideConds, ?Consequence)
%%    Parser:inference(+Grammar, ?Antecedents, ?SideConds, ?Consequence)
%%        - Grammar is the module where the grammar rules are
%%        - Antecedents is a comma-separated list of Items
%%        - SideConds is a combined Prolog goal (comma-list)
%%        - Consequence is the resulting Item
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% the following predicates are defined in this file:
%%
%%    deduct(+Parser, +Grammar)
%%        - builds a chart from the module Parser, given the module Grammar
%%
%%    deduct(+Parser, +Grammar, +Sentence)
%%        - same as deduct/2, but also encodes a sentecnce in the predicates
%%          scan/3 and sentencelength/1
%%    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(utilities, [remove_clauses/1]).

deduct(Parser, Grammar, Sentence) :-
	initialize(Sentence),
	deduct(Parser, Grammar).

deduct(Parser, Grammar) :-
	cleanup(Parser),
	process(Parser, Grammar).

cleanup(Parser) :-
	\+ (
	     Parser:item_form(Item),
	     remove_clauses(Item),
	     fail
	   ).

initialize(Sentence) :-
	remove_clauses(scan/3),
	remove_clauses(sentencelength/1),
	encode_sentence(Sentence, 0).

encode_sentence([Word | Words], From) :-
	To is From + 1,
	assert(scan(From, To, Word)),
	encode_sentence(Words, To).
encode_sentence([], Length) :-
	assert(sentencelength(Length)).

process(Parser, Grammar) :-
	\+ (
	     Parser:axiom(Grammar, SideConds, Item),
	     SideConds,
	     add_item(Parser, Grammar, Item),
	     fail
	   ).

add_item(Parser, Grammar, Item) :-
	\+ (
	     \+ Parser:subsuming_item(Item),
	     assert(Item),
	     consequence(Parser, Grammar, Item, Consequence),
	     add_item(Parser, Grammar, Consequence),
	     fail
	).

consequence(Parser, Grammar, Item, Consequence) :-
	Parser:inference(Grammar, Items, SideConds, Consequence),
	select_item(Items, Item),
	SideConds.

select_item((Left, Right), Item) :-
	!, ( select_item(Left, Item), Right
	   ; select_item(Right, Item), Left
	   ).
select_item(Item, Item).

