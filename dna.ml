open String 

type dna = string 

type t = (int, dna) Hashtbl.t 

exception Done

exception Malformed


let print_variant dna cur_count= 
  print_endline "print variant: ";
  print_endline "current counter: ";
  print_int cur_count; 
  match Hashtbl.find_opt dna 0 with 
  | None -> print_endline "None"; ()
  | Some h -> print_endline h; ()


(** [add_dna t str] is t with valid DNA sequences in str added. 
    Precondition: [str] is not empty *)
let rec add_dna str dna (cur_count:int ref) : unit = 
  let str = str |> uppercase_ascii in 
  let length = length str in 
  if length = 0 then () else 
  let rem_string = (sub str 1 (length - 1)) in 
  incr(cur_count);
  match (get str 0 |> Char.uppercase_ascii) with 
  | 'A' -> Hashtbl.add dna (!cur_count) "A";  add_dna rem_string dna cur_count
  | 'C' -> Hashtbl.add dna (!cur_count) "C"; add_dna rem_string dna cur_count
  | 'G' -> Hashtbl.add dna (!cur_count) "G"; add_dna rem_string dna cur_count
  | 'T' -> Hashtbl.add dna (!cur_count) "T"; add_dna rem_string dna cur_count
  | '-' -> Hashtbl.add dna (!cur_count) "N"; add_dna rem_string dna cur_count
  | '_' -> Hashtbl.add dna (!cur_count) "N"; add_dna rem_string dna cur_count
  | h -> add_dna rem_string dna cur_count  

let parse_line str (dna:t) counter : unit= 
  if length str = 0 then () else 
  let first_char = get str 0 in 
  if first_char = '>' || first_char = ' ' then () else 
  (add_dna str dna counter)

let parse_file f dna counter = 
  let in_channel = open_in f in 
  try 
    while true do
      let line = input_line in_channel in 
      parse_line line dna counter 
    done
  with 
  | End_of_file -> close_in in_channel
  | e -> close_in_noerr in_channel; raise Malformed

let from_fasta (f:string) : t = 
  let dna_sequence = Hashtbl.create 1048576 in 
  let counter = ref (-1) in 
  try parse_file f dna_sequence counter; dna_sequence  
  with Malformed -> raise Malformed 


let get (t:t) pos = 
  Hashtbl.find_opt t pos 

let is_empty t = 
  if Hashtbl.length t = 0 then true else false

  

  
  

