type dna = A | C | T | G | Gap | Mismatch

type t = string ref * (int, dna) Hashtbl.t 

exception Malformed

(** [extract_name str dna] extracts a name from str and mutates the 
    name of the dna_seq to the extracted name. *)
let extract_name str (dna_seq: t) : unit = 
  let line_length = String.length str in 
  let name = String.sub str 1 (line_length - 1) in 
  match dna_seq with 
    (n, tbl) -> n := name;
    ()

(** [add_dna str t cur_count] mutates t by adding valid DNA sequences 
    in str with counter cur_ref. *)
let rec add_dna str dna (cur_count:int ref) : unit = 
  let str = str |> String.uppercase_ascii in 
  let length = String.length str in 
  if length = 0 then () else 
    let rem_string = (String.sub str 1 (length - 1)) in 
    incr(cur_count);
    match (String.get str 0 |> Char.uppercase_ascii) with 
    | 'A' -> Hashtbl.add dna (!cur_count) A; add_dna rem_string dna cur_count
    | 'C' -> Hashtbl.add dna (!cur_count) C; add_dna rem_string dna cur_count
    | 'G' -> Hashtbl.add dna (!cur_count) G; add_dna rem_string dna cur_count
    | 'T' -> Hashtbl.add dna (!cur_count) T; add_dna rem_string dna cur_count
    | '-' -> Hashtbl.add dna (!cur_count) Gap; add_dna rem_string dna cur_count
    | '_' -> Hashtbl.add dna (!cur_count) Gap; add_dna rem_string dna cur_count
    | h -> add_dna rem_string dna cur_count  


(** [parse_line] parses the inputted [str] and calls add_dna to update
    t based on valid DNA inputs in [str] *)
let parse_line str (dna:t) counter : unit = 
  if String.length str = 0 then () else 
    let first_char = String.get str 0 in 
    if first_char = '>' || first_char = ' ' then extract_name str dna else 
      (add_dna str (snd dna) counter)

(** [parse_file f] reads file [f] line by line and uses [dna] and [counter] 
    to create t data structure. 
    dna and counter are modified. *)
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
  let dna_sequence = (ref "", Hashtbl.create 10485760) in 
  let counter = ref (-1) in 
  try parse_file f dna_sequence counter; dna_sequence  
  with Malformed -> raise Malformed 


let from_string (str: string) : t = 
  let dna = Hashtbl.create 10485760 in 
  let counter = ref (-1) in 
  add_dna str dna counter; 
  (ref "", dna)


let get (t:t) pos = 
  match Hashtbl.find (snd t) pos with 
  | A -> Some 'A' 
  | C -> Some 'C' 
  | G -> Some 'G' 
  | T -> Some 'T' 
  | Gap -> Some '_'
  | Mismatch -> Some 'M'
  | exception Not_found -> None

let get_e (t: t) pos = 
  match Hashtbl.find (snd t) pos with 
  | A ->  'A' 
  | C ->  'C' 
  | G ->  'G' 
  | T ->  'T' 
  | Gap -> '_'
  | Mismatch ->  'M'
  | exception Not_found -> raise Not_found

let is_empty (_, tbl) = 
  if Hashtbl.length tbl = 0 then true else false

let length (_, tbl) = 
  Hashtbl.length tbl

let get_name (name, _)  = !name 

let extract_names (dnas: t array) = 
  Array.map (fun (name, _) -> !name) dnas 

(** [str_range_helper t ] modifies b by reading the chars 
    from [t.start, t.end) and adding these values to b. *)
let str_range_helper t (b:Buffer.t) start finish = 
  for i = start to (finish -1) do 
    let v = get t i in 
    match v with 
    | Some h -> Buffer.add_char b h
    | None -> failwith "Invalid inputs"
  done 

let string_of_range t start finish = 
  let output = Buffer.create (finish - start) in 
  str_range_helper t output start finish; 
  Buffer.contents output

let to_string t = string_of_range t 0 (length t)

let mutate d pos (_, dna_seq) = 
  if Hashtbl.mem dna_seq pos = true 
  then Hashtbl.replace dna_seq pos d 
  else 
    raise (Invalid_argument "index out of bounds")

let append d ((_, dna_seq):t) = 
  let l = Hashtbl.length dna_seq in 
  Hashtbl.add dna_seq (l) d 

(* [update_bindings pos dna_seq] increments the index of all dna nucleotides 
    in [dna_seq] by 1 starting at position [pos]. *)
let update_bindings (pos:int) full_dna_seq = 
  let dna_seq = 
    match full_dna_seq with 
      (_, t) -> t
  in 
  let l = Hashtbl.length dna_seq in 
  let current_value = ref (Hashtbl.find dna_seq (pos)) in  
  let current_pos = ref (pos + 1) in 
  let next_value = if pos = (l -1) then ref A else
      ref (Hashtbl.find dna_seq (pos + 1)) in 
  for i = pos to (l-1) do 
    Hashtbl.replace dna_seq !current_pos !current_value; 
    current_value := !next_value; 
    incr(current_pos);
    try next_value := Hashtbl.find dna_seq !current_pos;
    with _ -> ()
  done  


let insert d pos full_dna_seq = 
  let dna_seq = 
    match full_dna_seq with 
      (_,t) -> t
  in 
  if pos = (Hashtbl.length dna_seq) 
  then append d full_dna_seq
  else 
    (update_bindings pos full_dna_seq; 
     Hashtbl.replace dna_seq pos d; 
    )

