open Tree
open Distance

(** [add_species s1 s2 checked unchecked] adds [s1] and [s2] as a new clade in 
    [checked]@[unchecked] (which I will refer to as the tree list) if they are not part of any trees in 
    [checked]@[unchecked]. Otherwise, the species that is not in the existing tree list is added to the 
    tree that contains the other species. *)
let rec add_species species i j acc = 
  let s1 = species.(i) in
  let s2 = species.(j) in
  match (List.assoc_opt i acc), (List.assoc_opt j acc) with
  | None, None -> 
    ((Stdlib.min i j), zip_no_params [leaf_no_params s1; leaf_no_params s2])::acc
  | Some t, None -> let r = List.remove_assoc i acc in 
    ((Stdlib.min i j ), zip_no_params [leaf_no_params s2; t])::r
  | None, Some t -> let r = List.remove_assoc j acc in 
    ((Stdlib.min i j ), zip_no_params [leaf_no_params s1; t])::r
  | Some t1, Some t2 ->
    let r = acc |> List.remove_assoc i |> List.remove_assoc j in 
    ((Stdlib.min i j), zip_no_params [t1; t2])::r

(* match unchecked with 
   | [] -> 
   (zip_no_params [leaf_no_params s1; leaf_no_params s2])::checked
   | h::t -> 
   if (mem s1 h) 
   then checked @ (zip_no_params [leaf_no_params s2; h]::unchecked)
   else if (mem s2 h)
   then checked @ (zip_no_params [leaf_no_params s1; h]::unchecked)
   else add_species s1 s2 (h::checked) t *)

let rec upgma_help dist species acc = 
  print_endline ("upgma_help called");
  if is_done dist then 
    (print_endline ("done dist");
     let tree_list = (List.rev_map (fun x -> snd x) acc) in
     if List.length tree_list = 1 then (List.hd tree_list)
     else if List.length tree_list > 1 then zip_no_params tree_list
     else failwith "Precondition violated in upgma_help")
  else
    (print_endline ("not done dist");
     let i, j = min_index dist in
     print_endline ("min_index dist called");
     let t = add_species species i j acc in
     print_endline ("added species");
     let d = combine i j dist in 
     print_endline ("combined");
     upgma_help d species t)

let upgma dist species = 
  match Array.length species with
  | 0 -> Tree.empty
  | 1 -> Tree.leaf_no_params species.(0)
  | _ -> upgma_help dist species []