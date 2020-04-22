open Tree
open Lexer

exception SyntaxError

type phylo = {
  name : string; 
  description : string;
  tree : Tree.t;
}

(** The empty phylogenetic tree. *)
let empty_phylo = {
  name = "empty";
  description = "empty tree";
  tree = Tree.empty
}

(** The type of a taxonomy. *)
type taxonomy = {
  id : string option;
  scientific_name : string;
}

(** The empty taxonomy. *)
let empty_taxonomy : taxonomy = {
  id = None;
  scientific_name = "Unnamed";
}

(** The type of clade attributes. *)
type clade_attr = {
  name : string option;
  rank : string option;
  confidence : float option;
  taxonomy : taxonomy option;
}

(** Empty clade attributes. *)
let empty_clade_attr = {
  name = None;
  rank = None; 
  confidence = None; 
  taxonomy = None
}

(** The type of a phyloXML start tag and its attributes. *)
type start_tag = {
  tag_name : token;
  str_attr : (string * string) list option;
  num_attr : (string * int) list option;
  bool_attr : (string * bool) list option;
}

(** The empty phyloXML start tag. *)
let empty_start_tag (t : token) : start_tag = {
  tag_name = t;
  str_attr = None;
  num_attr = None;
  bool_attr = None;
}

(** Ref that will contain the function that peeks at the next token. *)
let peek = ref (fun () -> EOF)

(** Ref that will contain the function that consumes the next token. *)
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

(** [is_valid_tag t] is true if [t] is the name of a valid phyloXML tag. *)
let is_valid_tag (t : token) : bool =
  match t with
  | Phylogeny | Name | Description
  | Clade | Rank | Confidence
  | Taxonomy | SciName | ID
  | Word _ -> true
  | _ -> false

(** [parse_words acc] is a string that represents the next consecutive [Word] or
    [Num] tokens in the current file. Note that the string may also represent
    other tokens that have acceptable string representations, such as [Name],
    [True], or [Dot], among others.
    Effects: Consumes [Word] and [Num] tokens in the current file until a 
    token representing a symbol (not including [Dot]) is reached. *)
let rec parse_words (acc : string) : string =
  let w = !peek () in
  match w with
  | Word s -> consume (Word s); 
    if acc <> "" then parse_words (acc ^ " " ^ s) else parse_words s
  | Num n -> consume (Num n);
    if acc <> "" then parse_words (acc ^ " " ^ (string_of_int n))
    else n |> string_of_int |> parse_words
  | Dot | True | Clade | Name | Description | Taxonomy | ID | Rank | Confidence 
    -> consume w;
    if acc <> "" then parse_words (acc ^ " " ^ (to_string w))
    else w |> to_string |> parse_words
  | _ -> acc

(** [add_str_assoc lst attr] is [lst] extended with the binding in [attr]. *)
let add_str_assoc (lst : (string * string) list option) 
    (attr : (string * string)) : (string * string) list option =
  match lst with
  | None -> Some [attr]
  | Some assoc -> Some (attr::assoc)

(** [add_bool_assoc lst attr] is [lst] extended with the binding in [attr]. *)
let add_bool_assoc (lst : (string * bool) list option)
    (attr : (string * bool)) : (string * bool) list option =
  match lst with
  | None -> Some [attr]
  | Some assoc -> Some (attr::assoc)

(** [add_int_assoc lst attr] is [lst] extended with the binding in [attr]. *)
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
  print_endline ("Consumed a " ^ (to_string t) ^ " end tag")

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
  | x when is_word x -> consume x; consume Eq; 
    let attr = to_string x in
    let new_tag =
      begin
        match (!peek ()) with
        | Quote -> consume Quote; 
          let return_tag =
            begin
              match (!peek ()) with
              | Word _ | Num _ -> let words = parse_words "" in 
                {tag with str_attr = add_str_assoc tag.str_attr (attr, words)}
              | True -> consume True; 
                {tag with bool_attr = add_bool_assoc tag.bool_attr (attr, true)}
              | False -> consume False;
                {tag with bool_attr = 
                            add_bool_assoc tag.bool_attr (attr, false)}
              | _ -> print_endline "SyntaxError ?"; raise SyntaxError
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

(** [parse_name ()] is the string held within a pair of phyloXML "name" tags.
    Requires: [parse_start_tag ()] to be called directly before calling this.
    Effects: Consumes tokens in the current file up until and including the 
    ending "name" phyloXML tag.
    Raises: [SyntaxError] if the ending phyloXML tag does not match [Name], or
    if there is invalid phyloXML syntax. *)
let parse_name () : string =
  print_endline "starting to parse name contents";
  match (!peek ()) with
  | Word _ | Num _ -> let name = parse_words "" in consume_end_tag Name; name
  | _ -> print_endline "SyntaxError: Name not word/number"; raise SyntaxError 

(** [parse_description ()] is the string held within a pair of phyloXML 
    "description" tags.
    Requires: [parse_start_tag ()] to be called directly before calling this.
    Effects: Consumes tokens in the current file up until and including the 
    ending "description" phyloXML tag.
    Raises: [SyntaxError] if the ending phyloXML tag does not match 
    [Description], or if there is invalid phyloXML syntax. *)
let parse_description () : string =
  match (!peek ()) with
  | Word _ | Num _ -> 
    let descr = parse_words "" in consume_end_tag Description; descr
  | _ -> print_endline "SyntaxError: Description not word/number";
    raise SyntaxError 

(** [parse_rank ()] is the string held within a pair of phyloXML "rank" tags.
    Requires: [parse_start_tag ()] to be called directly before calling this.
    Effects: Consumes tokens in the current file up until and including the 
    ending "rank" phyloXML tag.
    Raises: [SyntaxError] if the ending phyloXML tag does not match [Rank], if
    the rank does not represent a string, or if there is invalid phyloXML 
    syntax. *)
let parse_rank () : string = 
  match (!peek ()) with 
  | Word _ -> let rank = parse_words "" in consume_end_tag Rank; rank
  | _ -> print_endline "SyntaxError: Rank not a string"; raise SyntaxError 

(** [parse_confidence ()] is the float held within a pair of phyloXML 
    "confidence" tags.
    Requires: [parse_start_tag ()] to be called directly before calling this.
    Effects: Consumes tokens in the current file up until and including the 
    ending "confidence" phyloXML tag.
    Raises: [SyntaxError] if the ending phyloXML tag does not match 
    [Confidence], if the tokens do not represent a number, or if there is 
    invalid phyloXML syntax. *)
let parse_confidence () : float = 
  match (!peek ()) with 
  | Num n -> 
    consume (Num n);
    (if (!peek ()) = Dot 
     then 
       (consume Dot; 
        begin
          match (!peek ()) with 
          | Num x -> 
            let f = 
              float_of_string ((string_of_int n) ^ "." ^ (string_of_int x)) 
            in consume (Num x); consume_end_tag Confidence; f
          | _ -> 
            print_endline "Warning: confidence not valid. Set to default 0.0";
            consume_end_tag Confidence; 0.0
        end)
     else (consume_end_tag Confidence; float_of_int n))
  | Dot -> (consume Dot; 
            begin
              match (!peek ()) with 
              | Num x -> 
                let f = float_of_string ("0." ^ (string_of_int x)) 
                in consume (Num x); consume_end_tag Confidence; f
              | _ -> print_endline 
                       "Warning: confidence not valid. Set to default 0.0";  
                consume_end_tag Confidence; 0.0
            end)
  | _ -> print_endline "SyntaxError: Confidence not a number"; raise SyntaxError 

(** [parse_id ()] is the string held within a pair of phyloXML "id" tags.
    Requires: [parse_start_tag ()] to be called directly before calling this.
    Effects: Consumes tokens in the current file up until and including the 
    ending "id" phyloXML tag.
    Raises: [SyntaxError] if the ending phyloXML tag does not match [Id], or
    if there is invalid phyloXML syntax. *)
let parse_id () : string =
  match (!peek ()) with
  | Word _ | Num _ -> let name = parse_words "" in consume_end_tag ID; name
  | _ -> print_endline "SyntaxError: ID not word/number";
    raise SyntaxError 

(** [parse_scientific_name ()] is the string held within a pair of phyloXML 
    "scientific_name" tags.
    Requires: [parse_start_tag ()] to be called directly before calling this.
    Effects: Consumes tokens in the current file up until and including the 
    ending "scientific_name" phyloXML tag.
    Raises: [SyntaxError] if the ending phyloXML tag does not match [Name], or
    if there is invalid phyloXML syntax. *)
let parse_scientific_name () : string =
  match (!peek ()) with
  | Word _ | Num _ -> let name = parse_words "" in consume_end_tag SciName; 
    print_endline name; name
  | _ -> print_endline "SyntaxError: Scientific name not word/number";
    raise SyntaxError 

(** [parse_taxonomy ()] is the taxonomy represented within a pair of phyloXML
    "taxonomy" tags.
    Requires: [parse_start_tag ()] to be called directly before calling this.
    Effects: Consumes tokens in the current file up until and including the 
    ending "taxonomy" phyloXML tag.
    Raises: [SyntaxError] if the ending phyloXML tag does not match [Taxonomy], 
    or if there is invalid phyloXML syntax. *)
let rec parse_taxonomy (taxonomy : taxonomy) : taxonomy option =
  match (!peek ()) with
  | LAngle -> let tag = parse_start_tag () in
    begin
      match (tag.tag_name) with
      | ID -> parse_taxonomy
                {taxonomy with id = Some (parse_id ())}
      | SciName -> print_endline "Parsing scientific name";
        parse_taxonomy 
          {taxonomy with scientific_name = parse_scientific_name ()}
      | x -> ignore_tag x; parse_taxonomy taxonomy
    end
  | LAngleSlash -> consume_end_tag Taxonomy; Some taxonomy
  | _ -> print_endline "Unexpected token encountered when parsing taxonomy";
    raise SyntaxError

(** [parse_clade ()] is the clade represented within a pair of phyloXML
    "clade" tags.
    Requires: [parse_start_tag ()] to be called directly before calling this.
    Effects: Consumes tokens in the current file up until and including the 
    ending "clade" phyloXML tag.
    Raises: [SyntaxError] if the ending phyloXML tag does not match [Clade], 
    or if there is invalid phyloXML syntax. *)
let rec parse_clade (acc : Tree.t) (attr : clade_attr) : Tree.t =
  match (!peek ()) with 
  | LAngle -> print_endline "Parsing clade contents"; 
    let tag = parse_start_tag () in 
    begin
      match tag.tag_name with
      | Confidence -> 
        parse_clade acc {attr with confidence = Some (parse_confidence ())} 
      | Taxonomy -> 
        parse_clade acc {attr with taxonomy = parse_taxonomy empty_taxonomy} 
      | Name ->
        parse_clade acc {attr with name = Some (parse_name ())}
      | Clade -> 
        if is_empty acc then
          (print_endline "is_empty acc";
           parse_clade (parse_clade acc empty_clade_attr) attr)
        else
          (print_endline "acc not empty";
           let clade = parse_clade Tree.empty empty_clade_attr in
           parse_clade (zip_no_params [acc; clade]) attr)
      | x -> ignore_tag x; parse_clade acc attr
    end
  | LAngleSlash -> print_endline "langleslash"; consume_end_tag Clade; 
    begin
      match acc with
      | t when is_empty t -> 
        begin
          match attr.taxonomy with
          | Some taxon -> leaf taxon.scientific_name taxon.id attr.name
          | None -> 
            begin
              match attr.name with 
              | Some n -> leaf n None (Some n)
              | None -> print_endline "Warning: no name provided"; 
                leaf_no_params "Unnamed"
            end
        end
      | t -> 
        let id = 
          begin
            match attr.taxonomy with 
            | None -> None 
            | Some t -> t.id
          end in 
        print_endline ".."; zip [acc] attr.confidence attr.rank id attr.name
    end
  | _ -> print_endline "SyntaxError 7"; raise SyntaxError 

(** [parse_phylogeny ()] is the phylogeny represented within a pair of phyloXML
    "phylogeny" tags.
    Requires: [parse_start_tag ()] to be called directly before calling this.
    Effects: Consumes tokens in the current file up until and including the 
    ending "phylogeny" phyloXML tag.
    Raises: [SyntaxError] if the ending phyloXML tag does not match [Phylogeny], 
    or if there is invalid phyloXML syntax. *)
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

(** [from_phylo_helper file_stream] is the phylogenetic tree represented by
    the phyloXML in [file_stream]. *)
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