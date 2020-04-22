open OUnit2
open Phylo_parser

(** [doesParse f] is true if the phyloXML file pointed to by file 
    [f] is successfully parsed. 
    Requires: [f] is a phyloXML file within the PhyloXML folder. *)
let doesParse f =
  try 
    ignore(let p = from_phylo ("PhyloXML/" ^ f) in Tree.print_tree p.tree);
    true
  with _ -> false

let phylo1 = (from_phylo "PhyloXML/tree1.xml").tree
let rotated1 = (from_phylo "PhyloXML/test3.txt").tree
let phylo5 = (from_phylo "PhyloXML/test5.txt").tree
let phylo6 = (from_phylo "PhyloXML/test6.txt").tree

let rotated_trees = [
  "tree1, test3" >:: (fun _ -> assert_equal phylo1 rotated1 ~cmp:Tree.is_equal);
  "test5, test6" >:: (fun _ -> assert_equal phylo5 phylo6 ~cmp:Tree.is_equal);
]

let large_files = [
  "frog.xml" >:: (fun _ -> assert_equal true (doesParse "frog.xml"));
  "expanded_tree.txt" >:: (fun _ -> assert_equal true 
                              (doesParse "expanded_tree.txt"));
  "tol_156.xml" >:: (fun _ -> assert_equal true (doesParse "tol_156.xml"));
  "collapsed_tree.txt" >:: (fun _ -> assert_equal true 
                               (doesParse "collapsed_tree.txt"));
  "apaf.xml" >:: (fun _ -> assert_equal true (doesParse "apaf.xml"));
]

let tests =
  "test suite for lexer"  >::: List.flatten [
    rotated_trees;
    large_files;
  ]

let _ = run_test_tt_main tests