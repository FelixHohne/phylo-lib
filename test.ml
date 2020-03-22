let y = 2

let () = y |> print_int

(** [why are we doing this] is *)
let rec why are we doing this = 
  match are, we with
  | None, 0 -> 0
  | Some n, x -> why are (we - 1) doing this
  | None, x -> 1

let dumb_printer x = 
  x |> print_endline 

let () = dumb_printer "x"
let () = print_endline("Pandemic")

let success = "it_works!"