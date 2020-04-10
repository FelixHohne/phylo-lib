open Clustal

(** RI: The index of the first sequence is strictly less than the index of the 
    second sequence. As a result, if there is only one sequence, 
    t is empty. *)
type index = int * int 
type t = (index, float) Hashtbl.t

let dist msa gap = 
  let m = size msa in
  let n = seq_len msa in
  let dist_matrix = Hashtbl.create m in 

  (for i = 0 to (m - 1) do
     let diff : int ref = ref 0 in

     (for j = i + 1 to (m - 1) do

        (for k = 0 to (n - 1) do
           if (get_base i k msa) = 'N' ||  (get_base j k msa) = 'N' then
             diff := !diff + gap
           else if (get_base i k msa) <> (get_base j k msa) then
             incr(diff)
           else ()
         done);
        Hashtbl.add dist_matrix (i, j) (float_of_int !diff);
      done);
   done);
  dist_matrix

(** [min t] is a ((i,j), value) tuple of the minimum position 
    and value in [t].*)
let min dist = 
  if Hashtbl.length dist = 0 then ((0, 0), 0.)
  else
    let acc = ((0, 0), Float.max_float) in 
    Hashtbl.fold (fun k v acc -> if v < (snd) acc then (k, v) else acc) dist acc

let min_diff dist =  
  dist |> min |> snd

let min_index dist = 
  dist |> min |> fst

(** [remove i dist] removes all bindings in [dist] that contains [i] as 
    one of its indices. *)
let remove i dist =
  Hashtbl.filter_map_inplace 
    (fun k v -> if fst k = i || snd k = i then None else Some v) dist

let avg_helper j k dist = Hashtbl.find dist ((Stdlib.min j k), max j k) 

let average i j dist = 
  Hashtbl.filter_map_inplace 
    (fun k v -> 
       if fst k = i then 
         let other = snd k in 
         let v_val = v +. (avg_helper other j dist) in 
         Some (v_val /. 2.)
       else if snd k = i then
         let other = fst k in 
         Some ((v +. (avg_helper other j dist)) /. 2.)
       else Some v) dist

let combine i j dist = average i j dist; remove j dist; dist
