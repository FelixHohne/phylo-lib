open OUnit2
open Dna
open Tree
open Distance

let d1 = from_fasta "FASTA/example_2.fasta"
let d2 = from_fasta "FASTA/example_3.fasta"
let d3 = from_fasta "FASTA/example_4.fasta"
let d4 = from_fasta "FASTA/example_5.fasta"
let d5 = from_fasta "FASTA/example_6.fasta"
let aligned = Clustal.align [|d1; d2; d3; d4; d4;|]

let c1 = zip_no_params [leaf_no_params "A"; leaf_no_params "B"]
let c2 = zip_no_params [leaf_no_params "E"; leaf_no_params "F"]
let c3 = zip_no_params [c1; leaf_no_params "D"]
let tree1 = zip_no_params [c2; c3]
let tree2 = c2

let mat = dist aligned 1

let upgma1 = Phylo_algo.upgma mat [|"A"; "B"; "D"; "E"; "F"|]

let upgma_tests = [
  "tree 1" >:: (fun _ -> assert_equal upgma1 c2 ~cmp:Tree.is_equal)
]

let tests =
  "test suite for phylo algorithms"  >::: List.flatten [
    upgma_tests;
  ]

let _ = run_test_tt_main tests