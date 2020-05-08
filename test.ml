open OUnit2
include Distance_test
include Phylo_algo_test
include Tree_test
include Lexer_test
include Phylo_parser_test
include Dna_test
include Pairwise_test
include Msa_test

let phylo_suite = "test suite for sorts" >:::
                  [dna_tests;
                   distance_tests;
                   lexer_tests;
                   tree_tests;
                   pairwise_suite;
                   phylo_algo_tests;
                   phylo_parser_tests]

let _ = run_test_tt_main phylo_suite