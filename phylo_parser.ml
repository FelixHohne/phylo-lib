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

(** [consume token] consumes the next token in the file currently being 
    processed. 
    Raises: [SyntaxError] if the next token is not equal to [token]. *)
let consume (token : token) = 
  match (!peek ()) with
  | x when x = token -> ignore(!consume_token ())
  | x -> print_endline "SyntaxError 2";
    print_endline ("Expected " ^ to_string token ^ " but got " ^ to_string x);
    raise SyntaxError

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

(** [parse_words acc] is a string that represents the next consecutive [Word] 
    tokens in the current file. 
    Effects: Consumes [Word] tokens in the current file until a non-[Word] token
    is reached. *)
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

(** [consume_end_tag t] consumes the tokens representing an ending phyloXML tag, 
    for example, </clade>, in the file currently being processed.
    Raises: [SyntaxError] if the tokens consumed do not match the format of an
    ending tag or if the name of the ending tag does not equal [t]. *)
let consume_end_tag (t : token) : unit =
  consume LAngleSlash;
  consume t;
  consume RAngle;
  print_endline ("Just consumed a " ^ (to_string t) ^ " end tag")

(** [parse_start_tag ()] is a start_tag that represents the information
    gained from parsing a starting phyloXML tag that may have attributes, 
    for example, <phylogeny rooted="true">, in the file currently being
    processed. 
    Effects: consumes the tokens representing the start tag. *)
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
  | RAngle -> consume RAngle; 
    print_endline ("Consumed a " ^ (to_string tag.tag_name) ^ " start tag"); 
    tag
  | _ -> print_endline "SyntaxError 8"; raise SyntaxError

(** [ignore_tag t] consumes the tokens in the current file representing the 
    phyloXML that is nested within the phyloXML tag with name [t], including the
    ending tag.
    Requires: [parse_start_tag ()] to be called directly before calling this.
    Raises: [SyntaxError] if the name of the ending phyloXML tag does not match
    [t]. *)
let rec ignore_tag (t : token) : unit =
  match (!peek ()) with 
  | LAngle -> let tag = parse_start_tag () in 
    print_endline "The previous start tag was ignored";
    ignore_tag tag.tag_name; 
    ignore_tag t
  | LAngleSlash -> consume_end_tag t; 
    print_endline "The previous end tag was ignored"
  | x -> consume x; ignore_tag t

type confidence = {
  category : string
}

type id = {
  provider : string;
  num_id : int;
}

type taxonomy = {
  id : id option;
  scientific_name : string option;
}

type clade_attr = {
  rank : string option;
  confidence : confidence option;
  taxonomy : taxonomy option;
}

let empty_clade_attr = {
  rank = None; 
  confidence = None; 
  taxonomy = None
}

let parse_name () : string =
  failwith "Unimplemented"

let parse_description () : string =
  failwith "Unimplemented"

let parse_rank () : string = 
  failwith "Unimplemented"

let parse_confidence () : confidence = 
  failwith "Unimplemented"

let parse_id () : id =
  failwith "Unimplemented"

let parse_scientific_name () : string =
  failwith "Unimplemented"

let parse_taxonomy () : taxonomy =
  failwith "Unimplemented"

let rec parse_clade (acc : Tree.t) (attr : clade_attr) : Tree.t =
  match (!peek ()) with 
  | LAngle -> let tag = parse_start_tag () in 
    begin
      match tag.tag_name with
      | Taxonomy -> parse_clade acc {attr with taxonomy = Some (parse_taxonomy ())}
      | Clade -> parse_clade (parse_clade Tree.empty empty_clade_attr) attr
      | x -> ignore_tag x; parse_clade acc attr
    end
  | LAngleSlash -> consume_end_tag Clade; 
    begin
      match acc with
      | t when is_empty t -> leaf "Unimplemented"
      | t -> zip [acc; leaf "Unimplemented"]
    end
  | _ -> print_endline "SyntaxError 7"; raise SyntaxError 

let rec parse_phylogeny (acc : phylo) : phylo =
  match (!peek ()) with
  | LAngle -> let tag = parse_start_tag () in
    begin
      match tag.tag_name with
      | Name -> parse_phylogeny {acc with name = parse_name ()}
      | Description -> 
        parse_phylogeny {acc with description = parse_description ()}
      | Clade -> 
        parse_phylogeny {acc with tree = parse_clade acc.tree empty_clade_attr}
      | x -> 
        print_endline ("Ignoring " ^ (to_string x) ^ " in parse_phylogeny");
        ignore_tag x; parse_phylogeny acc
    end
  | LAngleSlash -> consume_end_tag Phylogeny; acc
  | _ -> print_endline "SyntaxError 6"; raise SyntaxError

let rec from_phylo_helper (f : string Stream.t )=
  let tokenizer = token_function_builder f in
  peek := tokenizer true;
  consume_token := tokenizer false;
  match (!peek ()) with
  | EOF -> empty_phylo
  | LAngle -> let tag = parse_start_tag () in 
    begin
      match tag.tag_name with
      | Word s when s = "phyloxml" -> 
        let tag = parse_start_tag () in
        begin
          match tag.tag_name with 
          | Phylogeny -> let phylo = parse_phylogeny empty_phylo in
            consume_end_tag (Word "phyloxml"); phylo
          | _ -> print_endline "SyntaxError 9"; raise SyntaxError
        end
      | _ -> print_endline "SyntaxError 5"; raise SyntaxError
    end
  | _ -> print_endline "SyntaxError 4"; raise SyntaxError

let from_phylo f = 
  f |> stream_of_file |> from_phylo_helper