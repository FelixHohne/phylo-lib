open OUnit2
open Phylo_printer

let trees = [
  "tree1, test3" >:: (fun _ -> assert_equal true true);
]

let phylo_printer_tests = 
  "test suite for parser" >::: List.flatten [
    trees
  ]