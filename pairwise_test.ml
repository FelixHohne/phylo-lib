(* 
#mod_use "dna.ml";;
let d1 = Dna.from_fasta "FASTA/pairwise_len_5.fasta";;
let d2 = Dna.from_fasta "FASTA/insert.fasta";;
#use "pairwise.ml";;
let d3 = Dna.from_string "ATGCATGAAC";;
let d4 = Dna.from_string "ATTCATGC";;
let a = align_pair d1 d2 1 (-2) (-5);;
let b = align_pair d3 d4 1 (-2) (-5);;
*)


open OUnit2
open Dna
open Pairwise

let d1 = Dna.from_fasta "FASTA/pairwise_len_5.fasta"
let d2 = Dna.from_fasta "FASTA/insert.fasta"
let a11 = "ATGCA"
let a12 = "AT_CG"
let align1 = Pairwise.align_pair d1 d2 1 (-2) (-5)

let d3 = Dna.from_string "ATGCATGAAC"
let d4 = Dna.from_string "ATTCATGC"
let a21 = "ATGCATGAAC" 
let a22 = "ATTCATG__C"
let align2 = Pairwise.align_pair d3 d4 1 (-2) (-5)


let d5 = Dna.from_string "TCGGTA" 
let d6 = Dna.from_string "TGGTGA"
let a31 = "TCGGT_A"
let a32 = "T_GGTGA"
let align3 = Pairwise.align_pair d5 d6 3 (-1) (-2)

let d7 = Dna.from_string "ATATA" 
let d8 = Dna.from_string "AAGTA"
let a41 = "ATA_TA"
let a42 = "A_AGTA"
let align4 = Pairwise.align_pair d7 d8 3 (-1) (-2)


let pair_wise_align = [
  "A11" >:: (fun _ -> assert_equal (Dna.to_string align1.(0)) a11);
  "A12" >:: (fun _ -> assert_equal (Dna.to_string align1.(1)) a12);
  "A21" >:: (fun _ -> assert_equal (Dna.to_string align2.(0)) a21); 
  "A22" >:: (fun _ -> assert_equal (Dna.to_string align2.(1)) a22); 
  "A31" >:: (fun _ -> assert_equal (Dna.to_string align3.(0)) a31); 
  "A32" >:: (fun _ -> assert_equal (Dna.to_string align3.(1)) a32); 
  "A41" >:: (fun _ -> assert_equal (Dna.to_string align4.(0)) a41); 
  "A42" >:: (fun _ -> assert_equal (Dna.to_string align4.(1)) a42); 
]

let suite = "test suite for sorts" >:::
            pair_wise_align

let _ = run_test_tt_main suite