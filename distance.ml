open Msa
open Pairwise

(** RI: The index of the first sequence is strictly less than the index of the 
    second sequence. As a result, if there is only one sequence, 
    t is empty. *)
type index = int * int 
type t = (index, float) Hashtbl.t

let dist_dna (dnas: Dna.t array) align misalign indel : t =
  let m = Array.length dnas in
  let dist_matrix = Hashtbl.create m in 
  (for i = 0 to (m - 1) do
     (for j = i + 1 to (m - 1) do
        Hashtbl.add dist_matrix (i, j) 
          (float_of_int (score dnas.(i) dnas.(j) align misalign indel))
      done);
   done);
  dist_matrix

let dist_msa (msa: Msa.t) (gap: int) : t = 
  let m = num_seq msa in
  let n = seq_len msa in
  let dist_matrix = Hashtbl.create m in 
  (for i = 0 to (m - 1) do
     let diff = ref 0 in
     (for j = i + 1 to (m - 1) do
        (for k = 0 to (n - 1) do
           if (get_base i k msa) = '_' ||  (get_base j k msa) = '_' then
             diff := !diff + gap
           else if (get_base i k msa) <> (get_base j k msa) then
             incr(diff)
         done);
        Hashtbl.add dist_matrix (i, j) (float_of_int !diff);
      done);
   done);
  dist_matrix

(** [min t] is a ((i,j), value) tuple of the minimum position 
    and value in [t]. *)
let min dist = 
  if Hashtbl.length dist = 0 then ((0, 0), 0.)
  else
    let acc = ((0, 0), Float.max_float) in 
    Hashtbl.fold (fun k v acc -> if v < snd acc then (k, v) else acc) dist acc

let min_diff (dist: t) : float =  
  let x = dist |> min |> snd in
  print_newline (); print_float x; print_newline (); x

let min_index dist : index = 
  let x = dist |> min |> fst in
  print_int (fst x); print_newline (); print_int (snd x); x

(** [remove i dist] removes all bindings in [dist] that contains [i] as 
    one of its indices. *)
let remove i dist =
  Hashtbl.filter_map_inplace 
    (fun k v -> if fst k = i || snd k = i then None else Some v) dist

(** [avg_helper j k dist] is the value stored at 
    indices [j] and [k] in [dist]. *)
let avg_helper j k dist =
  (* print_int j; print_newline (); print_int k; print_newline (); *)
  Hashtbl.find dist ((Stdlib.min j k), max j k) 

(** [average i j dist] iterates over the elements in matrix dist and updates 
    the entries where row = [i] or column = [i] to the average of 
    distance from current entry to i and distance from current entry to j. 
    Used for UPGMA. 
    See https://en.wikipedia.org/wiki/UPGMA for detailed algorithm.  *)
let average (i : int) (j : int) dist = 
  (* print_int i; print_newline (); print_int j; print_newline (); *)
  let avg (k : index) (v : float) = 
    if fst k = i then 
      let other = snd k in 
      let v_val = v +. (avg_helper other i dist) in 
      Some (v_val /. 2.)
    else if snd k = i then
      let other = fst k in 
      Some ((v +. (avg_helper other i dist)) /. 2.)
    else Some v
  in 
  Hashtbl.filter_map_inplace avg dist

let combine i j dist = 
  if i >= j 
  then failwith "invalid input"
  else 
    (* print_endline "calling avg"; *)
    average i j dist;
  (* print_endline "calling remove"; *)
  remove j dist; 
  (* print_endline "finished remove"; *)
  dist

let size dist = Hashtbl.length dist

let is_done dist = 
  (size dist = 0)
