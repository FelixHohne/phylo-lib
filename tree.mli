(** The abstract data type of values representing phylogenetic trees. *)
type t

(** The abstract data type of values representing a node in the phylo tree. *)
type node

(** The type of node identifiers. *)
type clade_id = int

exception UnknownClade of clade_id

(** The empty tree. *)
val empty : t

(** [is_empty t] is true if the tree is empty. *)
val is_empty : t -> bool


val add : t -> string -> string