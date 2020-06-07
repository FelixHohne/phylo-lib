open Tree
open Phylo_parser
open Printf

type attr = (string * string) list

(** A reference that will point to the out_channel for the file currently 
    being written to. It points to [stdout] when no files are being written. *)
let oc = ref stdout

(** [print_f s] prints [s] to the current file. *)
let print_f (s : string) : unit = 
  fprintf !oc "%s" s

(** [print_tabs n oc] prints [n] 2-space tabs to the current file. *)
let print_tabs (n : int) : unit =
  for x = 1 to n do print_f "  " done

let rec print_attr (attr : attr) : unit =
  match attr with 
  | [] -> ()
  | (k, v)::t -> 
    let space = (t <> []) in 
    print_f (k ^ "=\"" ^ v ^ "\"" ^ (if space then " " else ""));
    print_attr t

let print_start_tag (tag : string) (attr : attr) (tabs : int) (newline : bool) 
  : unit =
  print_tabs tabs;
  print_f ("<" ^ tag);
  print_attr attr;
  print_f (if newline then ">\n" else ">")

let print_end_tag (tag : string) (tabs : int) : unit =
  print_tabs tabs;
  print_f ("</" ^ tag ^ ">\n")

let print_inline_tag (tag : string) (tabs : int) (s : string) : unit =
  print_tabs tabs;
  print_f ("<" ^ tag ^ ">");
  print_f s;
  print_end_tag tag 0

(** [print_inline_opt tag tabs info] prints an inline tag [tag] with [tabs] 
    number of tabs to the current file if [info] carries information. *)
let print_inline_opt (tag : string) (tabs : int) (info : string option) : unit =
  match info with 
  | Some s -> print_inline_tag tag tabs s
  | None -> ()

let rec print_tree_helper (tree : Tree.t) (tabs : int) : unit =
  match tree with 
  | Clade info -> 
    print_start_tag "clade" [] tabs true;
    print_inline_opt "name" (tabs + 1) info.name;
    let confidence = 
      begin
        match info.bootstrap with 
        | Some bootstrap -> Some (string_of_float bootstrap)
        | None -> None
      end in
    print_inline_opt "confidence" (tabs + 1) confidence;
    (if (info.rank <> None || info.id <> None) 
     then 
       begin
         print_start_tag "taxonomy" [] (tabs + 1) true;
         print_inline_opt "id" (tabs + 2) info.id;
         print_inline_opt "rank" (tabs + 2) info.rank;
         print_end_tag "taxonomy" (tabs + 1);
       end
     else ());
    let rec print_children children tabs = 
      match children with 
      | [] -> ()
      | h::t -> print_tree_helper h tabs; print_children t tabs 
    in 
    print_children info.children (tabs + 1);
    print_end_tag "clade" tabs;
  | Leaf info -> 
    print_start_tag "clade" [] tabs true;
    print_inline_opt "name" (tabs + 1) info.name;
    (if info.scientific_name <> "" || info.id <> None 
     then 
       begin
         print_start_tag "taxonomy" [] (tabs + 1) true;
         begin
           if info.scientific_name <> ""
           then (print_inline_tag "scientific_name" (tabs + 2) 
                   info.scientific_name)
           else ()
         end;
         print_inline_opt "id" (tabs + 2) info.id;
         print_end_tag "taxonomy" (tabs + 1);
       end
     else ());
    print_end_tag "clade" tabs

(** [print_phylo_helper phylo] prints [phylo] to the current file. *)
let print_phylo_helper (phylo : phylo) : unit =
  print_start_tag "phyloxml" [] 0 true;
  print_start_tag "phylogeny" [] 1 true;
  print_tree_helper phylo.tree 2;
  print_end_tag "phylogeny" 1;
  print_end_tag "phyloxml" 0

let print_phylo_xml (phylo : phylo) (f : string) : unit = 
  failwith "Unimplemented"

let print_tree_xml (tree : Tree.t) (f : string) : unit = 
  oc := open_out f;
  let phylo = { name = ""; description = ""; tree = tree} in 
  print_phylo_helper phylo;
  close_out !oc;
  oc := stdout 