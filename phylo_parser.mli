open Tree
exception SyntaxError

(** [from_phylo string] parses the phyloXML file at the absolute path. 
    Requires: there is a valid phyloXML file at the location represented by 
    the string. *)

type phylo = {
  name : string; 
  description : string;
  tree : Tree.t;
}

val from_phylo : string -> phylo


