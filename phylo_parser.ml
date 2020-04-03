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
  | x when x = token -> ignore(!consume_token ())
  | _ -> print_endline "SyntaxError 2"; raise SyntaxError

let rec parse_name (t : Lexer.t) =
  failwith "Unimplemented"

type start_tag = {
  tag_name : token;
  str_attr : (string * string) list option;
  num_attr : (string * int) list option;
  bool_attr : (string * bool) list option;
}

let empty_start_tag (t : token) : start_tag = {
  tag_name = t;
  str_attr = None;
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

let rec parse_words (acc : string) : string =
  match (!peek ()) with
  | Word s -> consume (Word s); 
    if acc <> "" then parse_words (acc ^ " " ^ s) else parse_words s
  | _ -> acc

let add_str_assoc (lst : (string * string) list option) 
    (attr : (string * string)) : (string * string) list option =
  match lst with
  | None -> Some [attr]
  | Some assoc -> Some (attr::assoc)

let add_bool_assoc (lst : (string * bool) list option)
    (attr : (string * bool)) : (string * bool) list option =
  match lst with
  | None -> Some [attr]
  | Some assoc -> Some (attr::assoc)

let add_int_assoc (lst : (string * int) list option)
    (attr : (string * int)) : (string * int) list option =
  match lst with
  | None -> Some [attr]
  | Some assoc -> Some (attr::assoc)

let rec parse_start_tag () : start_tag =
  consume LAngle;
  let tag =
    (match (!peek ()) with
     | x when is_valid_tag x -> consume x; empty_start_tag x
     | _ -> print_endline "SyntaxError 1"; raise SyntaxError) in
  parse_attr tag
and
  parse_attr (tag : start_tag) : start_tag = 
  match (!peek ()) with
  | Word attr -> consume (Word attr); consume Eq; 
    let new_tag =
      begin
        match (!peek ()) with
        | Quote -> consume Quote; 
          let return_tag =
            begin
              match (!peek ()) with
              | Word _ -> let words = parse_words "" in 
                {tag with str_attr = add_str_assoc tag.str_attr (attr, words)}
              | True -> consume True; 
                {tag with bool_attr = add_bool_assoc tag.bool_attr (attr, true)}
              | False -> consume False;
                {tag with bool_attr = add_bool_assoc tag.bool_attr (attr, false)}
              | _ -> raise SyntaxError
            end
          in consume Quote; return_tag
        | Num x -> consume (Num x);
          {tag with num_attr = add_int_assoc tag.num_attr (attr, x)}
        | _ -> print_endline "SyntaxError 3"; raise SyntaxError
      end
    in parse_attr new_tag
  | RAngle -> consume RAngle; tag
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