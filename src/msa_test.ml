(* 
Utop testing script for Msa.to_string

let d1 = Dna.from_string "ACATA";;
let d2 = Dna.from_string "CATAA";;
let d3 = Dna.from_string "CATTT";;
let arr = [| d1; d2; d3 |];;
let msa = Msa.align arr;;
msa |> Msa.to_string |> print_string;;


let d1 = Dna.from_string "ACATAACATAACATAACATAACATAACATAACATAACATAACATAACATAACATAACATAACATAACATAACATAACATAACATA";;
let d2 = Dna.from_string "CATAACATAACATAACATAACATAACATAACATAACATAACATAACATAACATAACATAACATAACATAACATAACATAACATAA";;
let d3 = Dna.from_string "CATTTCATTTCATTTCATTTCATTTCATTTCATTTCATTTCATTTCATTTCATTTCATTTCATTTCATTTCATTTCATTTCATTT";;
let arr = [| d1; d2; d3 |];;
let msa = Msa.align arr;;
msa |> Msa.to_string |> print_string;; 

let a = Msa.align [||];;
Msa.to_string a;;

let empty = Dna.from_string "";;
let a = Msa.align [|empty|];;
Msa.to_string a;;
*)