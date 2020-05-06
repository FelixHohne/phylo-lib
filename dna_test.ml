open OUnit2
open Dna

let easy_example = from_fasta "FASTA/example.fasta"
let ex2 = from_fasta "FASTA/example_2.fasta"

let fruit_fly_x = from_fasta "FASTA/fruit_fly_x.fasta"
let fruit_fly_3r = from_fasta "FASTA/fruit_fly_3r.fasta"

let simple_ex = from_fasta "FASTA/simple_ex.fasta"

let name = from_fasta "FASTA/name_1.fasta"
let name2 = from_fasta "FASTA/name_2.fasta"

let names = [|"ferredoxin oxidoreductase"; 
              "Drosophila melanogaster chromosome 3R"|]

let mut = from_fasta "FASTA/insert.fasta"
let c_mut = from_fasta "FASTA/insert.fasta"

let create_DNA = [

  "easy 1 " >:: (fun _ -> assert_equal (get easy_example 0) (Some 'G'));
  "easy 2 " >:: (fun _ -> assert_equal (get easy_example 1) (Some 'A'));
  "easy 3 " >:: (fun _ -> assert_equal (get easy_example 5) (Some 'C'));
  "fruit fly 1 " >:: (fun _ -> assert_equal (get fruit_fly_x 0) (Some 'G')); 
  "fruit fly 2 " >:: (fun _ -> assert_equal (get fruit_fly_x 1) (Some 'A')); 
  "fruit fly 3" >:: (fun _ -> assert_equal (get fruit_fly_3r 0) (Some 'A'));
  "fruit fly 3" >:: (fun _ -> assert_equal (get fruit_fly_x 5) (Some 'C'));
  "fruit fly 4" >:: (fun _ -> assert_equal (get fruit_fly_3r 9) (Some 'A'));

]

let dna_functions = [
  "is_empty" >:: (fun _ -> assert_equal (is_empty ex2) (false));
  "length" >:: (fun _ -> assert_equal (length ex2) 13); 
  "str" >:: (fun _ -> assert_equal (string_of_range ex2 1 11) "AATTTCAAAC"); 
  "str2" >:: (fun _ -> assert_equal (string_of_range ex2 0 1) "G");
  "str2" >:: (fun _ -> assert_equal (string_of_range ex2 0 0) "");
  "str3" >:: (fun _ -> assert_equal (string_of_range ex2 1 3) "AA");
]

let more_dna = [
  "h" >:: (fun _ -> assert_equal (string_of_range simple_ex 24 32) "TCCTGCTG");
  "counter check" >:: (fun _ -> assert_equal (get ex2 7) (Some ('A'))); 
  "counter check2" >:: (fun _ -> assert_equal (get fruit_fly_3r 7) (Some ('C')));
]

let ins0 = from_fasta "FASTA/insert0.fasta"
let ins1 = from_fasta "FASTA/insert1.fasta"
let ins2 = from_fasta "FASTA/insert2.fasta"


let empty_seq = Dna.from_string "_A-"
let empty = Dna.from_string ""
let empty2 = Dna.from_string "hhh"


let bisect_gap = [

  "empty_seq" >:: (fun _ -> assert_equal (to_string empty_seq) "_A");
  "_" >:: (fun _ -> assert_equal (get_e empty_seq 0) '_');
  "empty" >:: (fun _ -> assert_equal (is_empty empty) true); 
  "empty" >:: (fun _ -> assert_equal (is_empty empty2) true); 
  "get" >:: (fun _ -> assert_equal (get empty_seq (-1)) None); 
]
let tests =
  "test suite for phylo_lib tree"  >::: List.flatten [
  create_DNA; 
  dna_functions; 
  more_dna; 
  bisect_gap;
  ]

let _ = run_test_tt_main tests