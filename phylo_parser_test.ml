open OUnit2
open Phylo_parser

(** This tests the Phylo_parser module, and also indirectly tests the Lexer 
    module.
    Black box tests proceed by parsing small or empty phyloXML files and 
    comparing the result to the trees that were expected to be produced.
    To increase the variety of inputs for the tests, several large phyloXML
    files were also parsed. Since it would be time-consuming to generate the 
    correct trees by hand, the [display_tree] flag is included to help visually
    verify the correctness of the parsed tree. The assumption is that if the
    parser can produce a tree that can be printed without producing errors,
    the parser likely will also successfully parse phyloXML files similar to
    the ones tested. *)

(** Whether trees will be printed by [does_parse]. *)
let display_tree = false

(** [does_parse f] is true if the phyloXML file pointed to by file 
    [f] is successfully parsed. 
    Requires: [f] is a phyloXML file within the PhyloXML folder.
    Effects: Prints tree to standard output if [display_tree] is true. *)
let does_parse f =
  try 
    let phylo = from_phylo ("PhyloXML/" ^ f) in
    if display_tree then Tree.print_tree phylo.tree else ();
    true
  with _ -> false

(** [tree_from_phylo f] is the phylogenetic tree represented by the phyloXML
    file [f].
    Requires: [f] is a phyloXML file within the PhyloXML folder. *)
let tree_from_phylo f = 
  let path_name = "PhyloXML/" ^ f in 
  (from_phylo path_name).tree

let phylo1 = tree_from_phylo "tree1.xml"
let rotated1 = tree_from_phylo "test3.xml"
let phylo5 = tree_from_phylo "test5.xml"
let phylo6 = tree_from_phylo "test6.xml"
let empty_phylo = tree_from_phylo "empty.xml"
let blank_phylo = tree_from_phylo "blank.xml"

let rotated_trees = [
  "tree1, test3" >:: (fun _ -> assert_equal phylo1 rotated1 ~cmp:Tree.is_equal);
  "test5, test6" >:: (fun _ -> assert_equal phylo5 phylo6 ~cmp:Tree.is_equal);
]

let empty_trees = [
  "empty_phylo" >:: (fun _ -> assert_equal empty_phylo Tree.empty 
                        ~cmp:Tree.is_equal);
  "blank_phylo" >:: (fun _ -> assert_equal blank_phylo Tree.empty 
                        ~cmp:Tree.is_equal);
]

let large_files = [
  "frog.xml" >:: (fun _ -> assert_equal true (does_parse "frog.xml"));
  "expanded_tree.txt" >:: (fun _ -> assert_equal true 
                              (does_parse "expanded_tree.xml"));
  "tol_156.xml" >:: (fun _ -> assert_equal true (does_parse "tol_156.xml"));
  "collapsed_tree.txt" >:: (fun _ -> assert_equal true 
                               (does_parse "collapsed_tree.xml"));
  "apaf.xml" >:: (fun _ -> assert_equal true (does_parse "apaf.xml"));
]

let phylo_parser_tests =
  "test suite for parser"  >::: List.flatten [
    rotated_trees;
    empty_trees;
    large_files;
  ]