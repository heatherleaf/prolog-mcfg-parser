-- Time-stamp: <2006-01-12, 14:01>

Prolog library of parsing algorithms for 
Multiple Context-Free Grammar and Grammatical Framework 
======================================================================

by Peter Ljunglöf (peb@cs.chalmers.se)

This library contains parsing algorithms for Multiple Context-Free Grammars,
as well as some utilities for handling parse trees, parse forests,
and other useful stuff.

The library can also handle files generated from the Grammatical Framework
(called GFC grammars here).

The library can be used for parsing Minimalist Grammars (MG),
by using the MG to MCFG translation by Matthieu Guillaumin.
For more information about MG and translations to MCFG, please visit:

          http://www.humnet.ucla.edu/humnet/linguistics/people/stabler/epssw.htm

For the latest version of the library, please visit my homepage:

          http://www.cs.chalmers.se/~peb


----------------------------------------------------------------------
-- Installation instructions:

The library should work without modifications

The supported Prolog versions are Sicstus and SWI


----------------------------------------------------------------------
-- List of files:

  deduct.pl
    - a general deduction engine, inspired by Shieber, Schabes & Pereira (1995)

  tree.pl
    - extracting syntax trees from a chart
    - generating syntax trees (randomly or deterministic) from a grammar

  forest.pl
    - extracting syntax forests from a chart
    - extracting syntax trees from a forest

  mcfg.pl
    - general definitions for MCFG and GFC
    - linearizing syntax trees to strings (and general linearization terms)
  
  mcfg2prolog.perl
    - a Perl script for converting MCFG grammar in a very simple text format 
      (used by e.g. Dan Albro in his OCaml MCFG parser)
      into the Prolog format used by this library

  optimize_active.pl
    - script for converting an MCFG grammar into a format
      optimized for the active algorithm in recognize_active.pl
    - assuming that the grammar is defined in file(s) gram1.pl..., 
      write one of the following lines at the unix prompt:

      % swipl -q -g main -f optimize_active.pl -- gram1.pl...

      % sicstus -f --goal 'main.' -l optimize_active.pl -a -- gram1.pl...

  recognize_active.pl
    - active bottom-up chart parser for MCFG as described in
      Ljunglöf (2004, sec 4.4) and Burden (2005, sec 3.4)

  utilities.pl
    - general utility predicates
    - some of these are specific to different Prolog implementations
      currently only SWI and Sicstus are supported
      (by files utilities_swi.pl and utilities_sicstus.pl)

  example-simple/simple.pl
    - a simple multilingual grammar for Swedish and English
  example-simple/simple_start.pl
    - the starting categories for the simple grammar

  example-larsonian/larsonian.mcfg
    - a quite advanced grammar by John Hale, 
      stolen from the examples for Dan Albro's MCFG parser in OCaml
  example-larsonian/larsonian.pl
    - how the Larsonian grammar looks like after converting it by mcfg2prolog.perl
  example-larsonian/larsonian_start.pl
    - defining the starting category for the Larsonian grammar
  example-larsonian/larsonian_sentences.pl
    - some example sentences for the Larsonian grammar

  example-gf-fragment/Fragment*.gf
    - a multilingual Grammatical Framework (GF) grammar with the same coverage as the simple grammar above
  example-gf-fragment/compile_fragment.gfscript
    - a GF script file for compiling the grammar into Prolog format
  example-gf-fragment/fragment_start.pl
    - starting categories for the compiled GF grammar


----------------------------------------------------------------------
-- Basic usage instructions

Here is an example run for the Simple multilingual grammar, which I hope is pedagogical:

1. Optimize the MCFG grammar for the parsing algorithm:

   % swipl -q -g main -f optimize_active.pl -- example-simple/simple.pl > example-simple/simple_optimized.pl

2. Start SWI Prolog (or Sicstus Prolog):

   % swipl
   Welcome to SWI-Prolog

3. Consult the grammar and the start symbols:

   ?- ['example-simple/simple_optimized'].
   ?- ['example-simple/simple_start'].

4. Consult the recognizer; 
   the predicates for extracting parse trees; 
   and the predicates for linearization/generation:

   ?- use_module(recognize_active).
   ?- use_module(tree).
   ?- use_module(mcfg].

5. Recognize a sentence (with the 'bu' strategy and the Simple English grammar); 
   then extract a parse tree:

   ?- recognize(bu, english, [many,lions,hunt,fish]).
   ?- extract_tree(Tree).
   Tree = s(np2(d_many, n_lion), vp(v_hunt, np1(n_fish)))

6. Translate a sentence, by parsing (in Swedish) and then linearizing (to English). 

   ?- recognize(bu, english, [many,lions,hunt,fish]), 
      extract_tree(Tree), 
      linearize(swedish, Tree, p1, Sentence).

   Tree = s(np2(d_many, n_lion), vp(v_hunt, np1(n_fish)))
   Sentence = [många, lejon, jagar, fiskar] ;

   Tree = s(np2(d_many, n_lion), vp(v_hunt, np1(n_fish)))
   Sentence = [jagar, många, lejon, fiskar] ;

   No more solutions


-- Using the Larsonian example grammar

1. The Larsonian grammar is in Dan Albro's MCFG format, so we first have to 
   convert the mcfg file to Prolog format:

   % perl mcfg2prolog.perl example-larsonian/larsonian.mcfg > example-larsonian/larsonian.pl

2. Then we can optimize the grammar, and recognize sentences:

   % swipl -q -g main -f optimize_active.pl -- example-larsonian/larsonian.pl > example-larsonian/larsonian_optimized.pl
   % swipl
   Welcome to SWI-Prolog
   ?- ['example-larsonian/larsonian_optimized'].
   ?- ['example-larsonian/larsonian_start'].
   ?- use_module(recognize_active).
   ?- use_module(tree).

   ?- recognize(bu, [the, right, answer, matter, '-s']).
   ?- extract_tree(Tree).

   Tree = 'S'('Bu'('Di'('BL', 'At'('As', 'As')), 'CO'('M'('Dt'(i, 'At'('As', 'As')), 'CB'('DI'('Du'('BU', 'At'('As', 'As')), 'H'(d, 'At'('As', 'As'))), t('DD'('V', 'At'('As', 'As')), 'AU'('De'('BI', 'At'('As', 'As')), 'W'('AE'('E', 'At'(..., ...)), 'W'('B', 'At'(..., ...))))))))))     

3. There is also a file of test sentences:

   ?- ['example-larsonian/larsonian_sentences.pl'].
   ?- sentence(Sent), recognize(bu, Sent), extract_tree(Tree).

4. Finally, there's a timing predicate in utilities.pl:

   ?- use_module(utilities).
   ?- sentence(Sent), trace_runtime(Sent, recognize(bu, Sent)), fail.
   [[the, boy, be, -s, young], runtime: 0.220 ms]
   [[the, boy, be, -s, so, young], runtime: 0.280 ms]
   [[the, right, answer, matter, -s], runtime: 0.150 ms]
   ...


-- Using the example GF grammar

You need to have Grammatical Framework (GF) installed for being able to use this grammar:

1. Download and install GF from:

   http://www.cs.chalmers.se/~aarne/GF/

2. Convert the GF grammars to MCFG and GFC format;
   this will create the files fragment_mcfg.pl and fragment_gfc.pl:

   % cd example-gf-fragment
   % gf < compile_fragment.gfscript
   % cd ..

3. Optimize the grammar, start Prolog and recognize sentences:

   % swipl -q -g main -f optimize_active.pl -- example-gf-fragment/fragment_mcfg.pl > example-gf-fragment/fragment_optimized.pl
   % swipl
   Welcome to SWI-Prolog
   ?- ['example-gf-fragment/fragment_optimized'].
   ?- ['example-gf-fragment/fragment_start'].
   ?- use_module(recognize_active).
   ?- use_module(tree).

   ?- recognize(bu, mcfg_FragmentEnglish, [many, lions, hunt, fish]).
   ?- extract_tree(Tree).

   Tree = s(np2(d_many, n_lion), vp(v_hunt, np1(n_fish)))

4. Consult the predicates for linearization/generation, plus the GFC linearization grammar;
   translate by parsing and then linearizing in GFC format:

   ?- [mcfg].
   ?- ['example-gf-fragment/fragment_gfc'].

   ?- recognize(bu, mcfg_FragmentEnglish, [many, lions, hunt, fish]).

   ?- extract_tree(Tree), 
      linearize_gfc(gfc_FragmentSwedish, Tree, [s,'Decl'^[]], Sentence).

   Tree = s(np2(d_many, n_lion), vp(v_hunt, np1(n_fish)))
   Sentence = [många, lejon, jagar, fiskar] 

   ?- extract_tree(Tree), 
      linearize_gfc(gfc_FragmentSwedish, Tree, [s,'Que'^[]], Sentence).

   Tree = s(np2(d_many, n_lion), vp(v_hunt, np1(n_fish)))
   Sentence = [jagar, många, lejon, fiskar] 


----------------------------------------------------------------------
-- Licence conditions

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

Please send bug reports, comments or suggestions to
peb@cs.chalmers.se
