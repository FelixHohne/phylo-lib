open Tree
open Lexer

exception SyntaxError
(** Supported tags: Tokens

    Things we need
    Tokens
    Tokenizer
    Parser

    1 is Shirley
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

let peek = ref (fun () -> EOF)
let consume_token = ref (fun () -> EOF)

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


(* We probably should write a recursive helper function. That is better OCaml style *)

let empty_phylo = 
  {
    name="empty";
    description="empty tree";
    tree= Tree.empty
  }


(* Would it not be easier to convert the tokens into an intermediary form that combine names with words, clades with floats...*)

(* Then we could convert these into trees more easily, rather than trying to do everything at once *)



(** Do we want to create an intermediary data structure during the parsing process or directly create trees? *)
(* We need to put statements together first, then check depth, then create trees. *)
(** Have helpers that deal with creating the clades / leaves, while top-level
    function only works with whole trees. *)

(** Where are we checking that the file is syntactically or semantically correct?*)
(* in the helper functions, like if you have a left angle then the helper function for that should expect a CLade/etc tag, and if it's something weird it throws an exception? *)
let consume (token:token) = 
  match (!peek ()) with
  | x when x = token -> ()
  | _ -> raise SyntaxError

let rec parse_name (t : Lexer.t) =
  failwith "Unimplemented"

let rec parse_start_tag (t : Lexer.t) =
  consume LAngle;
  match (t true) with
  | Name -> parse_name
  | _ -> raise SyntaxError

let rec from_phylo_helper f =
  let tokenizer = token_function_builder f in
  peek := tokenizer true;
  consume_token := tokenizer false;
  match (!peek ()) with
  | EOF -> empty_phylo
  | _ -> failwith "Unimplemented" (*parse_start_tag*)

let from_phylo f = 
  f |> stream_of_file |> from_phylo_helper
(* let peek x:unit = tokenizer true in
   let consume x:unit = tokenizer false *)


(* ___     ___
   | + |___| + |
   |           | 
   |__       __|
   |  \_____/  |
   ___|___________|___
   |                   |
   |___________________|
   \___________________/ 

*)