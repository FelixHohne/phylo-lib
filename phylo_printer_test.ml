open OUnit2
open Phylo_printer

let trees = [
  "tree1, test3" >:: (fun _ -> assert_equal true true);
]

let phylo_printer_tests = 
  "test suite for parser" >::: List.flatten [
    trees
  ]

(* 
#mod_use "tree.ml";;
#mod_use "phylo_parser.ml";;
#mod_use "phylo_printer.ml";;
let phylo = Phylo_parser.from_phylo "PhyloXML/collapsed_tree.xml";;
let tree = phylo.tree;;
Phylo_printer.print_tree_xml tree "testfile.xml";;
let phylo2 = Phylo_parser.from_phylo "PhyloXML/expanded_tree.xml";;
let tree2 = phylo2.tree;;
Phylo_printer.print_tree_xml tree2 "testfile2.xml";;
let phylo3 = Phylo_parser.from_phylo "testfile2.xml";;
Tree.print_tree tree2;;
Tree.print_tree phylo3.tree;;
*)