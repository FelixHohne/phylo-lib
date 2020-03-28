type clade_id = int
exception UnknownClade of clade_id

(** Represents a node in the phylogenetic tree *)
type t = Clade of {
    clade_id : clade_id;
    bootstrap : float option;
    children : t list;
  } | Leaf of {
    (* species_id : int; *)
    species : string;
  }

let empty = Clade { clade_id = 0; bootstrap = None ; children = []}

let leaf (species : string) : t =
  Leaf {species = species}

let is_empty t = (t == empty)

let rec add_species (tree : t) (clade_id : clade_id) (species : string) : t =
  match tree with
  | Leaf _ -> raise (UnknownClade clade_id)
  | Clade info ->
    if info.clade_id = clade_id
    then let new_sub_tree = Leaf {species = species} in
         Clade {info with children =  new_sub_tree::info.children}
    else add_species_helper info.children clade_id species
and
add_species_helper (lst: t list) (clade_id : clade_id) (species : string) : t =
  match lst with
  | [] -> raise (UnknownClade clade_id)
  | h::t -> try add_species h clade_id species
    with UnknownClade _ -> add_species_helper t clade_id species

let rec size (tree:t) = size_helper tree 0
and
size_helper tree (size: int) : int =
  match tree with
  | Leaf _ -> size + 1
  | Clade info -> (match info.children with
    | [] -> size
    | h::t ->
      1 + List.fold_left (fun acc x -> acc + (size_helper h size)) 0 (h::t))

let hierarchy a b = match a, b with
| Leaf t1, Leaf t2 -> compare t1.species t2.species
| Clade t1, Clade t2 -> Int.compare t1.clade_id t2.clade_id
| Leaf _, Clade _ -> -1
| Clade _, Leaf _ -> 1

let rec tlist_comp a b acc =
    if acc = false then false else match a, b with
    | [], [] -> true
    | h::t, p::q -> tlist_comp t q (is_equal h p)
    | _ -> false

and is_equal a b = match a, b with
| Leaf _, Leaf _ -> true
| Clade t1, Clade t2 -> let s1 = List.sort (hierarchy) t1.children in
  let s2 = List.sort (hierarchy) t1.children in tlist_comp s1 s2 true
| _ -> false

let zip (trees: t list) (id: clade_id) : t =
  Clade {clade_id = id; bootstrap = None; children = trees}
