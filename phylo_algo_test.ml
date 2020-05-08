open OUnit2
open Dna
open Tree
open Distance
open Phylo_algo

let d1 = from_fasta "FASTA/example_2.fasta"
let d2 = from_fasta "FASTA/example_3.fasta"
let d3 = from_fasta "FASTA/example_4.fasta"
let d4 = from_fasta "FASTA/example_5.fasta"
let d5 = from_fasta "FASTA/example_6.fasta"
let dnas = [|d1; d2; d3; d4; d5;|]

let c1 = zip_no_params [leaf_no_params "A"; leaf_no_params "B"]
let c2 = zip_no_params [leaf_no_params "E"; leaf_no_params "F"]
let c3 = zip_no_params [c1; leaf_no_params "D"]
let tree1 = zip_no_params [c2; c3]
let tree2 = zip_no_params [c2; leaf_no_params "D"]
let mat = dist_dna dnas 1 (-1) (-1)
let upgma1 = Phylo_algo.upgma mat [|"A"; "B"; "D"; "E"; "F"|]

let mat2 = dist_dna dnas 1 (-1) (-1)
let rotated1 = Phylo_algo.upgma mat2 [|"B"; "A"; "D"; "F"; "E"|]

let dnas2 = [|d3; d4; d5|]
let mat3 = dist_dna dnas2 1 (-1) (-1)
let upgma2 = Phylo_algo.upgma mat3 [|"D"; "E"; "F"|]
let dnas3 = [|d1; d2; d4; d5|]
let mat4 = dist_dna dnas3 1 (-1) (-1)
let upgma3 = upgma mat4 [|"A"; "B"; "E"; "F"|]
let tree3 = zip_no_params [c1; c2]

let emptyFails = 
  try ignore(let emptyAlign = (Msa.align [||]) in 
             let mat5 = (dist_msa emptyAlign 1) in 
             upgma mat5 [||]); false
  with _ -> true

let dnas4 = [|d1|]

let mat5 = dist_dna dnas4 1 (-1) (-1)
let upgma4 = upgma mat5 [|"A"|]

let upgma_tests = [
  "tree 1" >:: (fun _ -> assert_equal tree1 upgma1 ~cmp:Tree.is_equal);
  "rotated list" >:: (fun _ -> assert_equal upgma1 rotated1 ~cmp:Tree.is_equal);
  "tree 2" >:: (fun _ -> assert_equal tree2 upgma2 ~cmp:Tree.is_equal);
  "tree 3" >:: (fun _ -> assert_equal tree3 upgma3 ~cmp:Tree.is_equal);
  "empty" >:: (fun _ -> assert_equal true emptyFails);
  "leaf" >:: (fun _ -> assert_equal (leaf_no_params "A") upgma4)
]

let tests =
  "test suite for phylo algorithms"  >::: List.flatten [
    upgma_tests;
  ]

let _ = run_test_tt_main tests