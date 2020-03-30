(** The abstract data type of values representing phylogenetic trees. 
Representation invariant: the phylogenetic tree never contains two 
species with the same name. *)
type t

(* (** The abstract data type of values representing a node in the phylo tree.
    Can represent both clades and species.  *)
type node *)

(** The type of clade node identifiers. *)
type clade_id = int

(** The clade_id [clade_id] could not be found in this phylogenetic tree *)
exception UnknownClade of clade_id

(** The empty phylogenetic tree. *)
val empty : t

(** [is_empty t] is true iff. the tree is empty. *)
val is_empty : t -> bool

(** [add_species t clade_id species] adds [species] as a child of 
[clade_id] to phylogenetic tree [t]. Throws [UnknownClade clade_id] if clade_id
cannot be found in the phylogenetic tree. *)
val add_species: t -> clade_id -> string -> t 

(** [size t] is the size of tree t including both clodes and species.  *)
val size: t -> int 

(** [zip trees id] is the tree with a clade of [clade_id] as the root and
[trees] as the children.  *)
val zip: t list -> clade_id -> t

(** [leaf species] is the tree consisting of only [species]*)
val leaf: string -> t

(** [is_equal a b] is true if [a] and [b] are structurally similar, ignoring
the order of the children*)
val is_equal: t -> t -> bool

(** [print_tree t] prints an ASCII-art [t] to console semi-prettily. *)
val print_tree: t -> unit
