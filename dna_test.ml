open OUnit2
open Dna

let easy_example = from_fasta "FASTA/example.fasta"
let fruit_fly = from_fasta "FASTA/fruit_fly.fasta"

let create_DNA = [

  "easy 1 " >:: (fun _ -> assert_equal (get easy_example 0) (Some "G"));
  "easy 2 " >:: (fun _ -> assert_equal (get easy_example 1) (Some "A"));
  "easy 3 " >:: (fun _ -> assert_equal (get easy_example 5) (Some "C"));
  "fruit fly 1 " >:: (fun _ -> assert_equal (get fruit_fly 0) (Some "G")); 
  "fruit fly 2 " >:: (fun _ -> assert_equal (get fruit_fly 1) (Some "A")); 
  "fruit fly 3" >:: (fun _ -> assert_equal (get fruit_fly 5) (Some "C"));

]
let tests =
  "test suite for phylo_lib tree"  >::: List.flatten [
    create_DNA;
  ]

let _ = run_test_tt_main tests