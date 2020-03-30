open OUnit2
open Dna

let easy_example = from_fasta "FASTA/example.fasta"
let ex2 = from_fasta "FASTA/example_2.fasta"
let ex3 = from_fasta "FASTA/example_2.fasta"

let fruit_fly_x = from_fasta "FASTA/fruit_fly_x.fasta"
let fruit_fly_3r = from_fasta "FASTA/fruit_fly_3r.fasta"


let create_DNA = [

  "easy 1 " >:: (fun _ -> assert_equal (get easy_example 0) (Some "G"));
  "easy 2 " >:: (fun _ -> assert_equal (get easy_example 1) (Some "A"));
  "easy 3 " >:: (fun _ -> assert_equal (get easy_example 5) (Some "C"));
  "fruit fly 1 " >:: (fun _ -> assert_equal (get fruit_fly_x 0) (Some "G")); 
  "fruit fly 2 " >:: (fun _ -> assert_equal (get fruit_fly_x 1) (Some "A")); 
  "fruit fly 3" >:: (fun _ -> assert_equal (get fruit_fly_3r 0) (Some "A"));
  "fruit fly 3" >:: (fun _ -> assert_equal (get fruit_fly_x 5) (Some "C"));
  "fruit fly 4" >:: (fun _ -> assert_equal (get fruit_fly_3r 9) (Some "A"));

]


let dna_functions = [

  "is_empty" >:: (fun _ -> assert_equal (is_empty ex2) (false));
  "length" >:: (fun _ -> assert_equal (length ex2) 13); 
  (* "phys_equals" >:: (fun _ -> assert_equal (phys_equals ex2 ex3) false); *)
  "str" >:: (fun _ -> assert_equal (string_of_range ex2 1 11) "AATTTCAAAC"); 
  "str2" >:: (fun _ -> assert_equal (string_of_range ex2 0 1) "G");
  "str2" >:: (fun _ -> assert_equal (string_of_range ex2 0 0) "");
  "str3" >:: (fun _ -> assert_equal (string_of_range ex2 1 3) "AA");


]
let tests =
  "test suite for phylo_lib tree"  >::: List.flatten [
    create_DNA;
    dna_functions;
  ]

let _ = run_test_tt_main tests