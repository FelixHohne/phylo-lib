(** The abstract data type of values representing phylogenetic trees. 
Representation invariant: the phylogenetic tree never contains two 
species with the same name. *)
type t

(** The abstract data type of values representing a node in the phylo tree.
    Can represent both clades and species.  *)
type node

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
