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
let phylo = Phylo_parser.from_phylo "PhyloXML/small_tree.xml";;
Phylo_printer.print_tree_xml phylo.tree "testfile.xml";;
Tree.print_tree phylo.tree;;

let phylo2 = Phylo_parser.from_phylo "PhyloXML/collapsed_tree.xml";;
Phylo_printer.print_tree_xml phylo2.tree "testfile2.xml";;
Tree.print_tree phylo2.tree;;
let phylo_printed = Phylo_parser.from_phylo "testfile.xml";;
Tree.print_tree phylo_printed.tree;;

let phylo2 = Phylo_parser.from_phylo "PhyloXML/expanded_tree.xml";;
let tree2 = phylo2.tree;;
Phylo_printer.print_tree_xml tree2 "testfile2.xml";;
let phylo3 = Phylo_parser.from_phylo "testfile2.xml";;
Tree.print_tree tree2;;
Tree.print_tree phylo3.tree;;

#mod_use "tree.ml";;
#mod_use "phylo_parser.ml";;
#mod_use "phylo_printer.ml";;
let phylo = Phylo_parser.from_phylo "PhyloXML/small_tree.xml";;
let tree = phylo.tree;;
*)