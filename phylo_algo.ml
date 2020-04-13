open Tree
open Distance

(** [add_species s1 s2 checked unchecked] adds [s1] and [s2] as a new clade in 
    [checked]@[unchecked] (which I will refer to as the tree list) if they are not part of any trees in 
    [checked]@[unchecked]. Otherwise, the species that is not in the existing tree list is added to the 
    tree that contains the other species. *)
let rec add_species s1 s2 checked unchecked = 
  match unchecked with 
  | [] -> 
    (zip_no_params [leaf_no_params s1; leaf_no_params s2])::checked
  | h::t -> 
    if (mem s1 h) 
    then checked @ (zip_no_params [leaf_no_params s2; h]::unchecked)
    else if (mem s2 h)
    then checked @ (zip_no_params [leaf_no_params s1; h]::unchecked)
    else add_species s1 s2 (h::checked) t

let rec upgma_help dist species (acc: Tree.t list) = 
  let i, j = min_index dist in add_species species.(i) species.(j) [] acc




let upgma dist species = failwith "Unimplemented"