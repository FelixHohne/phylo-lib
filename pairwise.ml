open Dna

(** [max_thre a b c] is the largest of a, b, and c. *)
let max_three a b c = max (max a b) c

(** [init_matrix d1 d2 indel m n ] is an initalized matrix of size 
    m * n with the row and column headers initialized 
    according to the [indel] penalty. *)
let init_matrix (d1:Dna.t) (d2:Dna.t) (indel:int) (m:int) (n:int)  =
  let mat = Array.make_matrix m n Int.min_int in
  for r = 0 to m - 1 do
    mat.(r).(0) <- r * indel
  done;
  for c = 0 to n - 1 do
    mat.(0).(c) <- c * indel
  done;
  mat

(** [fill_matrix d1 d2 mat align misalign indel] is a filled-in matrix of 
    dimensions m by n that uses the [align], [misalign], 
    and [indel] parameters. *)
let fill_matrix d1 d2 align misalign indel m n =
  let mat = init_matrix d1 d2 indel m n in
  for r = 1 to m - 1 do 
    for c = 1 to n - 1 do
      let left = indel + mat.(r).(c-1) in
      let up = indel + mat.(r -1).(c) in
      let diagonal = match Dna.get d1 (r-1), Dna.get d2 (c-1) with
        | Some i, Some j -> mat.(r-1).(c-1) + if i = j then align else misalign
        | _ -> failwith "this should never happen" in
      mat.(r).(c) <- max_three left up diagonal
    done;
  done;
  mat

let align_pair d1 d2 align misalign indel = 
  let m = (Dna.length d1) + 1 in
  let n = (Dna.length d2) + 1 in
  let mat = fill_matrix d1 d2 align misalign indel m n in
  let r = ref (m - 1) in 
  let c = ref (n - 1) in
  let acc1 = ref "" in
  let acc2 = ref "" in
  while (!r > 0 || !c > 0) do
    let left = if !c < 1 then Int.min_int else indel + mat.(!r).(!c - 1) in
    let up = if !r < 1 then Int.min_int else indel + mat.(!r - 1).(!c) in
    let diagonal = (if !r < 1 || !c < 1 then min_int else
                      (mat.(!r-1).(!c-1) + 
                      (if Dna.get_e d1 (max 0 !r-1) = Dna.get_e d2 (max 0 !c-1) 
                      then align 
                      else misalign)))
    in
    let cell = mat.(!r).(!c) in
    (if cell = diagonal then
       begin
         acc1 := Char.escaped(Dna.get_e d1 (!r - 1))^ (!acc1);
         acc2 := Char.escaped(Dna.get_e d2 (!c - 1))^ (!acc2);
         decr r; decr c;
       end
     else if cell = left then 
       begin
         acc1 := "_" ^ !acc1; 
         acc2 := Char.escaped(Dna.get_e d2 (!c - 1)) ^ (!acc2);
         decr c
       end
     else if cell = up then 
       begin
         acc2 := "_" ^ !acc2; 
         acc1 := Char.escaped(Dna.get_e d1 (!r - 1))^ (!acc1);
         decr r
       end
     else failwith "This should not happen");
  done;
  [|Dna.from_string !acc1; Dna.from_string !acc2|]