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