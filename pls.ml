type clade_id = int
exception UnknownClade of clade_id

(** Represents a node in the phylogenetic tree *)
type t = Clade of {
    clade_id : clade_id;
    bootstrap : float option;
    children : t list;
  } | Leaf of {
    (* species_id : int; *)
    species : string;
  }

(** [print_spaces n] prints [n] spaces to the console. *)
let print_spaces (n : int) : unit =
  for x = 1 to n do print_char ' ' done

let rec print_vert_helper (ds : int list) (pos : int) (end_str : string) : unit =
  match ds with
  | [] -> ()
  | h::t -> 
    if h = pos && h <> 0 then print_spaces 1 else print_spaces (2 * (h - pos)); 
    if t = [] then print_string end_str
    else print_char '|'; print_vert_helper t (h + 1) end_str

(** [print_verts ds] prints vertical bars at the depths specified in [ds], 
    followed by a newline. 
    Requires: [ds] is sorted in descending order.
    Example: [print_verts [2;1;0]] would output "| | |" to the console,
    followed by a newline. *)
let print_verts (ds : int list) : unit =
  print_vert_helper (List.rev ds) 0 "|";
  print_endline ""

(** [print_branch ds] prints vertical bars at the depths specified in [ds], but
    replaces the last level with the symbol "O-". 
    Requires: [ds] is sorted in descending order.
    Example: [print_branch [2;1;0]] would output "| | O-" to the console. *)
let print_branch (ds : int list) : unit =
  match ds with
  | [] -> invalid_arg "ds"
  | _::t -> print_vert_helper (List.rev ds) 0 "O--"

let rec print_tree_helper (t_lst : t list) (d : int) (ds : int list) (spaces : bool) : unit = 
  match t_lst with 
  | [] -> ()
  | h::t -> 
    begin
      match h with
      | Leaf info -> info.species |> print_endline
      | Clade info -> 
        begin
        print_string "C\n";
        let new_ds = if t <> [] then d::ds else (
          match ds with 
            | [] -> []
            | h::t -> (d::t)
        ) in
        if new_ds <> [] then print_verts new_ds else print_newline ();
        if t <> [] then print_branch (d::ds) else (
          match ds with 
            | [] -> print_string "\n"
            | h::t -> print_branch (d::t)
        );
        print_tree_helper info.children (d+1) new_ds false
        end
    end

(* let rec print_tree_helper (t_lst : t list) (d : int) (ds : int list) (spaces : bool) : unit = 
  match t_lst with 
  | [] -> ()
  | h::t -> 
    begin
      match h with
      | Leaf info -> info.species |> print_endline
      | Clade info -> ("?-" |> print_string;
        begin
          match info.children with
          | [] -> ()
          | lh::lt -> 
            print_string "bg";
            print_tree_helper [lh] (d + 1) (d::ds) false;
            print_string "nd";
            if List.length lt > 0 then (print_verts (d::ds); print_branch (d::ds))
            else ();
            let new_ds = if List.length lt > 1 then d::ds else ds in
            print_tree_helper lt (d + 1) new_ds true 
        end;
        if t <> [] then
          begin
            print_verts ds;
            print_branch (ds);
            print_string "bg";
            print_tree_helper t d (d::ds) true;
            print_string "nd";
          end
        else ())
    end *)

(** [print_tree t] prints [t] to console semi-prettily. *)
let print_tree (t : t) : unit = 
  print_tree_helper [t] 0 [] false

  