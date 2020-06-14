open OUnit2
open Tree
open Phylo_parser
open Phylo_printer

(** Whether trees will be printed by [make_tree_printer_test]. *)
let display_tree = false

(** Name of the file that trees will be printed to in 
    [make_tree_printer_test] and [make_phylo_printer_test]. *)
let test_file = "testfile.xml"

(** [parsed_printed_trees_equal n f] constructs an OUnit test named [n] that 
    parses the phyloXML file [f] in the PhyloXML directory to generate a tree 
    [tr_1], then prints it to file using [print_tree_xml], and parses the 
    newly-printed file to generate a tree [tr_2], afterwards asserting the 
    equality of [tr_1] and [tr_2]. *)
let make_tree_printer_test n f = 
  let tr_1 = (from_phylo ("PhyloXML/" ^ f)).tree in 
  print_tree_xml tr_1 test_file;
  let tr_2 = (from_phylo test_file).tree in 
  if display_tree then (print_tree tr_1; print_tree tr_2)
  else ();
  n >:: (fun _ -> assert_equal tr_1 tr_2 ~cmp:Tree.is_equal)

(** [phylo_is_equal p1 p2] is true if phylogenies [p1] and [p2] are equal,
    including structural equality of their trees and equality of their names
    and descriptions. *)
let phylo_is_equal p1 p2 = 
  let tree_eq = Tree.is_equal p1.tree p2.tree in 
  tree_eq && p1.name = p2.name && p1.description = p2.description

let make_phylo_printer_test n f = 
  let p1 = from_phylo ("PhyloXML/" ^ f) in 
  print_phylo_xml p1 test_file;
  let p2 = from_phylo test_file in 
  n >:: (fun _ -> assert_equal p1 p2 ~cmp: phylo_is_equal)

let trees = [
  make_tree_printer_test "empty" "empty.xml"; 
  make_tree_printer_test "small" "small_tree.xml"; 
  make_tree_printer_test "collapsed" "collapsed_tree.xml";
  make_tree_printer_test "expanded" "expanded_tree.xml";
  make_tree_printer_test "frog" "frog.xml";
  make_tree_printer_test "many_frogs" "many_frogs.xml";
  make_tree_printer_test "tol" "tol_156.xml";
]

let phylos = [
  make_phylo_printer_test "empty_phylo" "empty.xml";
  make_phylo_printer_test "collapsed_phylo" "collapsed_tree.xml";
  make_phylo_printer_test "frog_phylo" "frog.xml";
  make_phylo_printer_test "many_frogs_phylo" "many_frogs.xml";
  make_phylo_printer_test "tol_phylo" "tol_156.xml";
]

let phylo_printer_tests = 
  "test suite for parser" >::: List.flatten [
    trees;
    phylos;
  ]

(* 
  Phylo_printer play-testing script. For use in utop after running make.

  let phylo = Phylo_parser.from_phylo "PhyloXML/small_tree.xml";;
  Phylo_printer.print_tree_xml phylo.tree "testfile.xml";;
  Tree.print_tree phylo.tree;;

  let phylo2 = Phylo_parser.from_phylo "PhyloXML/collapsed_tree.xml";;
  Phylo_printer.print_tree_xml phylo2.tree "testfile.xml";;
  Tree.print_tree phylo2.tree;;
  let phylo_printed = Phylo_parser.from_phylo "testfile.xml";;
  Tree.print_tree phylo_printed.tree;;

  let phylo2 = Phylo_parser.from_phylo "PhyloXML/expanded_tree.xml";;
  let tree2 = phylo2.tree;;
  Phylo_printer.print_tree_xml tree2 "testfile.xml";;

  let phylo3 = Phylo_parser.from_phylo "testfile.xml";;
  Tree.print_tree tree2;;
  Tree.print_tree phylo3.tree;;
*)