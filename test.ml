(** 
   Testing Strategy 
   The core functionality of our phylogenetic tree library works by passing 
   input through various modules, ultimately producing the desired output.
   In order to test such a structure, we primarily used unit tests to test 
   individual modules, and then performed integration testing in utop. 

   Each core module has its own unit tests, which test each part of the module 
   using a black box testing strategy. We then ran bisect on our code to make 
   sure our code coverage is reasonably comprehensive and haven't completely 
   failed to execute key lines of code. Our emphasis here was on particularly 
   tricky sections with small examples. For example, when testing pairwise.ml, 
   which implements the Needleman-Wunsch algorithm, we ran it on different 
   sequences and then compared it to our implementation. These unit tests 
   proved particulary useful when we refactored our code to ensure we did not 
   re-introduce old bugs.

   Once our key modules had been completed, we began performing integration 
   testing in utop. To test PhyloParser, we started reading in very large .
   PhyloXML files and asked it to lex, parse, and construct n-ary trees. We 
   then compared these trees to the inputted phyloXML files. On the tree 
   construction side, we inputted the DNA files of various influenza viruses 
   and coronaviruses and ensured that it constructed the right trees (i.e. 
   SARS is more closely related to SARS-COV-V2 than H1N1. Doing so tested DNA, 
   pairwise, distance, phylo_algo, and tree all altogether. We also wrote 
   programs that did the same thing using BioPython, Python's official 
   biological computation library. These programs produced the same results 
   that our code did, further reconfirming correctness.

   Another part of this process was testing performance. We tested DNA with DNA 
   sequences up to ~20 million base pairs, phyloParser with phyloXML files 
   several thousand lines of length, and pairwise with several sequences of 
   several thousand base pairs in length.
*)


open OUnit2
include Distance_test
include Phylo_algo_test
include Tree_test
include Lexer_test
include Phylo_parser_test
include Dna_test
include Pairwise_test
include Msa_test

let phylo_suite = "test suite for phyo_lib" >:::
                  [dna_tests;
                   distance_tests;
                   lexer_tests;
                   tree_tests;
                   pairwise_suite;
                   phylo_algo_tests;
                   phylo_parser_tests]

let _ = run_test_tt_main phylo_suite
