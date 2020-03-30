type clade_id = int
exception UnknownClade of clade_id

(** Represents a node in the phylogenetic tree. *)
type t = Clade of {
    clade_id : clade_id;
    bootstrap : float option;
    children : t list;
  } | Leaf of {
    (* species_id : int; *)
    species : string;
  }

(** True if the representation invariant is being checked. *)
let debug = true

(** [clade_ids t] is a list of all the clade_ids in [t]. *)
let rec clade_ids (tree : t)= 
  match tree with
  | Clade info -> 
    info.clade_id::
    (List.flatten (List.map (fun x -> clade_ids x) info.children))
  | Leaf _ -> []

(** [clade_ids_uniq t] is true if all the clade_ids within [t] are unique. *)
let clade_ids_uniq t =
  let all_clade_ids = clade_ids t in 
  List.sort_uniq compare all_clade_ids = List.sort compare all_clade_ids

(** [rep_ok t] checks the representation invariant for [t] if the flag [debug] 
    is true. It is [t] if [t] satisfies the representation invariant, otherwise,
    it raises [Failure] with a message that indicates what part of the rep 
    invariant was broken. *)
let rep_ok t =
  if not debug then t else
    (if clade_ids_uniq t then t else failwith "clade_ids not unique")

(** [id ()] is a counter for clade_ids to be called by functions inside this
    module whenever a new clade is constructed. *)
let id =
  let counter = ref 0 in 
  fun () -> 
    incr counter;
    !counter

let empty = Clade { clade_id = id (); bootstrap = None ; children = []}

let is_empty = function
  | Clade info -> info.bootstrap = None && info.children = []
  | Leaf _ -> false

let leaf (species : string) : t =
  Leaf {species = species}

(** [add_species t clade_id species] adds [species] as a child of 
    [clade_id] to phylogenetic tree [t]. Throws [UnknownClade clade_id] if 
    clade_id cannot be found in the phylogenetic tree. *)
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
  size_helper (tree: t) (size: int) : int = 
  match tree with 
  | Leaf _ -> size + 1
  | Clade info -> (match info.children with 
      | [] -> size
      | h::t ->
        1 + List.fold_left (fun acc x -> acc + (size_helper x size)) 0 (h::t))

let zip (trees: t list) : t = 
  Clade {clade_id = id (); bootstrap = None; children = trees} |> rep_ok

(** [hierarchy a b] is a comparator for [a] and [b]. *)
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
  
(** [print_spaces n] prints [n] spaces to the console. *)
let print_spaces (n : int) : unit =
  for x = 1 to n do print_char ' ' done

(** [print_vert_helper ds pos end_str] prints [end_str] at the depths specified 
  in [ds].
    Requires: [ds] is sorted in ascending order.
    [pos] is the depth last printed
   *)
let rec print_vert_helper (ds: int list) (pos: int) (end_str: string): unit =
  match ds with
  | [] -> ()
  | h::t -> 
    if h = pos && h <> 0 then print_spaces 1 
    else print_spaces (2 * (h - pos) + (if h = 0 || pos = 0 then 0 else 1));
    if t = [] then print_string end_str
    else print_char '|'; print_vert_helper t (h + 1) end_str

(** [print_verts ds] prints vertical bars at the depths specified in [ds], 
    followed by a newline. 
    Requires: [ds] is sorted in descending order.
    Example: [print_verts [2;1;0]] would output "| | |" to the console,
    followed by a newline. *)
let print_verts (ds : int list) : unit =
  print_vert_helper (List.rev ds) 0 "|";
  print_endline ""

(** [print_branch ds] prints vertical bars at the depths specified in [ds], but
    replaces the last level with the symbol "O-". 
    Requires: [ds] is sorted in descending order.
    Example: [print_branch [2;1;0]] would output "| | ∘-" to the console. *)
let print_branch (ds : int list) : unit =
  match ds with
  | [] -> invalid_arg "ds"
  | _::t -> print_vert_helper (List.rev ds) 0 "∘-"

let rec print_tree_helper (t_lst : t list) (d : int) (ds : int list): unit = 
  match t_lst with 
  | [] -> ()
  | h::t -> 
    begin
      match h with
      | Leaf info -> begin
        if ds <> [] then print_verts ds else print_newline ();
        if ds <> [] then print_branch ds;
      info.species |> print_endline;
      print_tree_helper t d ds
      end
      | Clade info -> 
        begin
        let new_ds = if t <> [] then d::ds else (
          match ds with 
            | [] -> [d]
            | h::t -> (d::t)
        ) in
        if ds <> [] then print_verts ds else ();
        if ds <> [] then print_branch ds else ();
        print_string "C\n";
        print_tree_helper info.children (d+1) new_ds;
        print_tree_helper t d ds;
        end;
    end

let print_tree (t : t) : unit = 
  print_tree_helper [t] 0 []

and 
  is_equal a b = match a, b with
  | Leaf _, Leaf _ -> true
  | Clade t1, Clade t2 -> let s1 = List.sort (hierarchy) t1.children in
    let s2 = List.sort (hierarchy) t1.children in tlist_comp s1 s2 true
  | _ -> false
