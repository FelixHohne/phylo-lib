type tree = Node of string * tree list

let tree_a : tree = Node ("M", 
                          [Node ("A", [Node ("C", []); Node ("D", [])]); 
                           Node ("Z", [])])

let tree_b : tree = Node ("A", [])

let tree_c : tree = Node ("M", [Node ("A", [Node ("C", []); 
                                            Node ("D", []); 
                                            Node ("E", [])]); 
                                Node ("S", [Node ("P", []); 
                                            Node ("Q", [])])])

let tree_d : tree = Node ("M", [Node 
                                  ("A", [Node ("C", []); 
                                         Node ("D", [Node ("F", []); 
                                                     Node ("G", [])]); 
                                         Node ("E", [])]); 
                                Node ("S", [Node ("P", [Node ("V", [])]); 
                                            Node ("Q", [])])])

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
  | _::t -> print_vert_helper (List.rev ds) 0 "O-"

let rec print_tree_helper (t_lst : tree list) (d : int) (ds : int list) (spaces : bool) : unit = 
  match t_lst with 
  | [] -> ()
  | h::t -> 
    begin
      match h with
      | Node (s, lst) -> 
        (if lst <> [] then (s ^ "-" |> print_string)
         else s |> print_endline);
        begin
          match lst with
          | [] -> ()
          | lh::lt -> 
            print_tree_helper [lh] (d + 1) (d::ds) false;
            if List.length lt > 0 then (print_verts (d::ds); print_branch (d::ds))
            else ();
            let new_ds = if List.length lt > 1 then d::ds else ds in
            print_tree_helper lt (d + 1) new_ds true 
        end;
        if t <> [] then
          begin
            print_verts ds;
            print_branch (ds);
            print_tree_helper t d (d::ds) true
          end
        else ()
    end

(** [print_tree t] prints [t] to console semi-prettily. *)
let print_tree (t : tree) : unit = 
  print_tree_helper [t] 0 [] false