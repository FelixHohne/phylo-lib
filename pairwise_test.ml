(* 
#mod_use "dna.ml";;
let d1 = Dna.from_fasta "FASTA/pairwise_len_5.fasta";;
let d2 = Dna.from_fasta "FASTA/insert.fasta";;
#use "pairwise.ml";;
let mat = fill_matrix d1 d2 1 (-2) (-5);;
*)