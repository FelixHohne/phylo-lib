(** The abstract data type of values representing phylogenetic trees. 
    Representation invariant: the phylogenetic tree never contains two 
    species with the same name. In any given tree, all clade_ids must be 
    unique.*)
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

(** [leaf species] is the tree consisting of only [species]. *)
val leaf: string -> t

(** [size t] is the size of tree t including both clades and species. *)
val size: t -> int 

(** [zip trees] is the tree with a clade as the root and [trees] as the 
    children. *)
val zip: t list -> t

(** [is_equal a b] is true if [a] and [b] are structurally similar, ignoring
    the order of the children. *)
val is_equal: t -> t -> bool

(** [print_tree t] prints an ASCII-art [t] to console semi-prettily. *)
val print_tree: t -> unit
