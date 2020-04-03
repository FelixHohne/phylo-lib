open Tree
open Lexer

exception SyntaxError
(** Supported tags: Tokens

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

let empty_phylo = {
  name = "empty";
  description = "empty tree";
  tree = Tree.empty
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

(** [consume token] consumes the next token in the file currently being 
    processed. 
    Raises: [SyntaxError] if the next token is not equal to [token]. *)
let consume (token : token) = 
  match (!peek ()) with
  | x when x = token -> ignore(!consume_token ()); ()
  | _ -> raise SyntaxError

let rec parse_name (t : Lexer.t) =
  failwith "Unimplemented"

type start_tag = {
  tag_name : token;
  string_attr : (string * string) list option;
  num_attr : (string * int) list option;
  bool_attr : (string * bool) list option;
}

let empty_start_tag (t : token) : start_tag = {
  tag_name = t;
  string_attr = None;
  num_attr = None;
  bool_attr = None;
}

(** [is_valid_tag t] is true if [t] is the name of a valid phyloXML tag. *)
let is_valid_tag (t : token) : bool =
  match t with
  | Phylogeny | Name | Description
  | Clade | Rank | Confidence
  | Taxonomy | SciName | ID
  | Word _ -> true
  | _ -> false

let parse_start_tag (t : Lexer.t) : start_tag =
  consume LAngle;
  match (!peek ()) with
  | x when is_valid_tag x -> empty_start_tag x
  | _ -> raise SyntaxError

let rec from_phylo_helper (f : string Stream.t )=
  let tokenizer = token_function_builder f in
  peek := tokenizer true;
  consume_token := tokenizer false;
  match (!peek ()) with
  | EOF -> empty_phylo
  | _ -> failwith "Unimplemented"

let from_phylo f = 
  f |> stream_of_file |> from_phylo_helper