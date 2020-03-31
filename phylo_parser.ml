open Tree
(** Supported tags: Tokens 

    Things we need
    Tokens
    Tokenizer
    Parser

    1 is Shirley
     ___     ___
    | + |___| + |
    |           | 
    |__       __|
    |  \_____/  |
    ___|___________|___
    |                   |
    |___________________|
    \___________________/
    2 is Vaish
    3 is Felix

    1
    <phylogeny rooted = true/false>
    2
    <name>
    3
    <description>

    1
    <clade>
    Include ranks, but don't pretty-print them. They are 
    used for internal nodes.
    2
    <rank>
    3
    <confidence>

    1
    <taxonomy>
    2
    <scientific_name>
    3
    <id>

*)

type phylo = {
  name : string; 
  description : string;
  tree : Tree.t;
}

(** [to_string t] is a string representing [t]. *)
(* let to_string (t : token) : string = 
   match t with 
   | Phylogeny b -> "Phylogeny: " ^ (if b then "Rooted" else "Unrooted")
   | Name -> "Name"
   | Description -> "Description"
   | Clade -> "Clade"
   | Rank -> "Rank"
   | Confidence -> "Confidence"
   | Taxonomy -> "Taxonomy"
   | SciName -> "SciName"
   | Id -> "Id" *)

let from_phylo str = 
  failwith "Unimplemted"
