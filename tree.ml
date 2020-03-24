type clade_id = int
exception UnknownClade of clade_id

(** Represents a node in the phylogenetic tree *)
type node = Clade of {
    clade_id : clade_id;
    bootstrap : float option;
  } | Leaf of {
    (* species_id : int; *)
    species : string;
  }

type t = Node of (node * t list)

let empty = Node (Clade { clade_id = 0; bootstrap = None }, [])


let rec add_species (tree : t) (clade_id : clade_id) (species : string) : t = 
  match tree with
  | Node (Leaf _, lst) -> raise (UnknownClade clade_id)
  | Node (Clade info, lst) -> 
    if info.clade_id = clade_id 
    then let new_species = Leaf {species = species} in 
         let new_sub_tree = Node (new_species, []) in 
         Node (Clade info, new_sub_tree::lst)
    else add_species_helper lst clade_id species
and 
add_species_helper (lst: t list) (clade_id : clade_id) (species : string) : t =
  match lst with
  | [] -> raise (UnknownClade clade_id)
  | h::t -> try add_species h clade_id species
    with UnknownClade _ -> add_species_helper t clade_id species
