# prolog-mcfg-parser

This is a library of Prolog files for parsing Multiple Context-Free Grammars.

**Note 1**: This is a programming library! Good knowledge of Prolog, and of Multiple Context-Free Grammars, is required to be able to use it.

**Note 2**: The library only works for Sicstus Prolog 3. Someday I might port it to SWI prolog.

Here can be found implementations of some of the algorithms I describe in chapter 4 of my PhD Thesis (2004). The algorithms are implemented using deductive parsing, in the spirit of Shieber, Schabes and Pereira (1995) and Sikkel (1997).

There are also utilities for extracting syntax trees and forests from the chart; and for linearization (i.e., generation of strings from syntax trees). The grammars can be multilingual, meaning that this can be used as a simple translation system.

There is also support for Grammatical Framework (GF) grammars, meaning that the library can be used as a Prolog API for parsing, linearization and translation of GF grammars. For more information about GF, please visit:

> <http://grammaticalframework.org/>

(Note 3: the GF support is broken since the release of GF 3. However, the internal GF parser by Krasimir Angelov is much more efficient than this library anyhow).

Finally, the library can be used for parsing Minimalist Grammars (MG), by using the MG to MCFG translation by Matthieu Guillaumin. For more information about MG and translations to MCFG, please visit:

> <http://www.humnet.ucla.edu/humnet/linguistics/people/stabler/epssw.htm>

## References

Peter Ljunglöf (2004). *Expressivity and Complexity of the Grammatical Framework*. PhD Thesis, University of Gothenburg and Chalemrs University of Technology.

Shieber, Schabes and Pereira (1995). *Principles and Implementation of Deductive Parsing*.

Sikkel (1997). *Parsing Schemata*.
