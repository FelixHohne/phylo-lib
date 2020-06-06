open Tree
open Phylo_parser

(** [print_phylo_xml phylo file] writes the phyloXML representation of [phylo] 
    to a file named [file]. *)
val print_phylo_xml : phylo -> string -> unit

(** [print_tree_xml tree file] writes the phyloXML representation of [tree] 
    to a file named [file]. *)
val print_tree_xml : Tree.t -> string -> unit

