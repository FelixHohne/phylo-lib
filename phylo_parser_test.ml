open OUnit2
open Phylo_parser

(** This tests the Phylo_parser module, and also indirectly tests the Lexer 
    module.
    Black box tests proceed by parsing small phyloXML files and 
    comparing the result to the trees that were expected to be produced.
    To increase the variety of inputs for the tests, several large phyloXML
    files were also parsed. Since it would be time-consuming to generate the 
    correct trees by hand, the [display_tree] flag is included to help visually
    verify the correctness of the parsed tree. The assumption is that if the
    parser can produce a tree that can be printed without producing errors,
    the parser likely will also successfully parse phyloXML files similar to
    the ones tested. *)

(** Whether trees will be printed by [doesParse]. *)
let display_tree = false

(** [doesParse f] is true if the phyloXML file pointed to by file 
    [f] is successfully parsed. 
    Requires: [f] is a phyloXML file within the PhyloXML folder.
    Effects: Prints tree to standard output if [display_tree] is true. *)
let doesParse f =
  try 
    let phylo = from_phylo ("PhyloXML/" ^ f) in
    if display_tree then Tree.print_tree phylo.tree else ();
    true
  with _ -> false

let phylo1 = (from_phylo "PhyloXML/tree1.xml").tree
let rotated1 = (from_phylo "PhyloXML/test3.xml").tree
let phylo5 = (from_phylo "PhyloXML/test5.xml").tree
let phylo6 = (from_phylo "PhyloXML/test6.xml").tree

let rotated_trees = [
  "tree1, test3" >:: (fun _ -> assert_equal phylo1 rotated1 ~cmp:Tree.is_equal);
  "test5, test6" >:: (fun _ -> assert_equal phylo5 phylo6 ~cmp:Tree.is_equal);
]

let large_files = [
  "frog.xml" >:: (fun _ -> assert_equal true (doesParse "frog.xml"));
  "expanded_tree.txt" >:: (fun _ -> assert_equal true 
                              (doesParse "expanded_tree.xml"));
  "tol_156.xml" >:: (fun _ -> assert_equal true (doesParse "tol_156.xml"));
  "collapsed_tree.txt" >:: (fun _ -> assert_equal true 
                               (doesParse "collapsed_tree.xml"));
  "apaf.xml" >:: (fun _ -> assert_equal true (doesParse "apaf.xml"));
]

let tests =
  "test suite for lexer"  >::: List.flatten [
    rotated_trees;
    large_files;
  ]

let _ = run_test_tt_main tests