open Tree
exception SyntaxError

type phylo = {
  name : string; 
  description : string;
  tree : Tree.t;
}

type start_tag

(** [from_phylo string] parses the phyloXML file at the absolute path. 
    Requires: there is a valid phyloXML file at the location represented by 
    the string. *)
val from_phylo : string -> phylo

