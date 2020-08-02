open Dna

type t = Dna.t array

let align dnas = dnas

let num_seq msa = Array.length msa

let seq_len msa = 
  Dna.length msa.(0)

let get_seq i msa = 
  msa.(i)

let get_base i j msa = 
  match Dna.get (get_seq i msa) j with
  | None -> failwith "Index out of bounds"
  | Some c -> c

let to_string msa = 
  let m = Array.length msa in
  if m = 0 then ""
  else 
    let n = Dna.length msa.(0) in
    if n = 0 then ""
    else
      let output = ref "" in
      let print_output s = output := !output ^ s in 
      for i = 0 to (n - 1) / 80 do
        for j = 0 to (m - 1) do 
          print_output 
            (Dna.string_of_range msa.(j) (80 * i) (min n (80 * (i + 1))));
          print_output "\n";
        done;
        if i + 1 > (n - 1) / 80 then ()
        else print_output "\n";
      done;
      !output