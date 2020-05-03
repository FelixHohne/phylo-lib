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

let mat = dist_dna dnas 1 (-1) (-1)
let mat2 = dist_dna dnas 1 (-1) (-1) |> combine 0 1
let mat3 = dist_dna dnas 1 (-1) (-1) |> combine 0 1 |> combine 0 2 |>combine 0 3
let mat4 = 
  dist_dna dnas 1 (-1) (-1) |> combine 0 1 |> combine 0 2 |> combine 0 3 |> 
  combine 0 4

let tests =
  "test suite for distance"  >::: [
    "min diff" >:: (fun _ -> assert_equal (-1.0) (min_diff mat));
    "min index 1" >:: (fun _ -> assert_equal (0, 1) (min_index mat));
    "min index 2 and combine" >:: 
    (fun _ -> assert_equal (3, 4) (min_index mat2)); 
    "min diff 2 and combine" >:: (fun _ -> assert_equal (-2.0) (min_diff mat2)); 
    "is_done false" >:: (fun _ -> assert_equal false (is_done mat2)); 
    "min index 3 and combine" >:: 
    (fun _ -> assert_equal (0, 4) (min_index mat3)); 
    "is_done false 2" >:: (fun _ -> assert_equal false (is_done mat3));
    "is_done true" >:: (fun _ -> assert_equal true (is_done mat4)); 
  ]

let _ = run_test_tt_main tests