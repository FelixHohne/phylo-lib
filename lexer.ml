exception EOF

type token =
| Phylogeny | Name | Description
| Clade | Rank | Confidence
| Taxonomy | SciName | ID
| LAngle | Slash | RAngle | Quote | Eq | Num of int | Dot
| Word of string | True | False  


let word_token_map = Hashtbl.create 16 
let () = Hashtbl.add word_token_map "phylogeny" Phylogeny;
Hashtbl.add word_token_map "name" Name;
Hashtbl.add word_token_map "description" Description;
Hashtbl.add word_token_map "clade" Clade;
Hashtbl.add word_token_map "rank" Rank;
Hashtbl.add word_token_map "confidence" Confidence;
Hashtbl.add word_token_map "taxonomy" Taxonomy;
Hashtbl.add word_token_map "scientific_name" SciName;
Hashtbl.add word_token_map "id" ID;
Hashtbl.add word_token_map "true" True;
Hashtbl.add word_token_map "false" False

(** [line_stream_of_file f] converts a file named [f] to a stream of strings. *)
let stream_of_file (f : string) : string Stream.t =
  let in_channel = open_in f in 
  Stream.from (fun _ ->
      try Some (input_line in_channel) with End_of_file -> None)

(** [stream_of_line stream] is a character stream of the next line of 
    string stream [stream]. *)
let stream_of_line (stream : string Stream.t) : char Stream.t = 
  match (Stream.next stream) with
  | str -> Stream.of_string str
  | exception Stream.Failure -> print_endline "End of file"; raise EOF

(** [peek_stream_of_line stream] *)
let peek_stream_of_line (stream : string Stream.t) : char Stream.t = 
  match (Stream.peek stream) with
  | Some str -> Stream.of_string str
  | None -> raise EOF

let rec is_special_char (c : char) : bool =
  match c with 
  | '<' 
  | '/'
  | '>' 
  | '"'
  | '='-> true
  | _ -> false


let rec lex_keyword (stream : char Stream.t) (acc : string) : token =
  match Stream.peek stream with 
  | Some t ->
    begin
      match t with
      | ' ' 
      | exception Stream.Failure -> (if Hashtbl.mem word_token_map acc
            then Hashtbl.find word_token_map acc else Word acc)
      | c when (is_special_char c) -> (if Hashtbl.mem word_token_map acc
            then Hashtbl.find word_token_map acc else Word acc)
      | c -> if Hashtbl.mem word_token_map acc
            then Hashtbl.find word_token_map acc 
            else (Stream.junk stream;
            lex_keyword stream (acc ^ (Char.escaped c)))
    end
  | None -> (if Hashtbl.mem word_token_map acc
          then Hashtbl.find word_token_map acc else Word acc)

(** [lex_number stream acc] lexes a number *)
let rec lex_number (stream : char Stream.t) (acc : string) : token =
  let x = Stream.peek stream in
  match x with 
  | Some c when (not (is_special_char c))->
    (try c |> Char.escaped |> int_of_string |> ignore; Stream.junk stream; lex_number (stream) (acc ^ (Char.escaped c))
    with _ -> Num (int_of_string acc))
  | Some c -> Num (int_of_string acc)
  | None -> Num (int_of_string acc)

(** [is_number n] is true if n is a number from 0 to 9 *)
let is_number (n : char) = 
try n |> Char.escaped |> int_of_string |> ignore; true
with _ -> false

(** [tokenize_line stream] is a tokenized list of [stream] *)
let rec tokenize_line (stream : char Stream.t) (acc : token list): token list = 
  match Stream.next stream with 
  | '<' -> tokenize_line stream (LAngle::acc) 
  | '/' -> tokenize_line stream (Slash::acc)
  | '>' -> tokenize_line stream (RAngle::acc)
  | '"' -> tokenize_line stream (Quote::acc)
  | '=' -> tokenize_line stream (Eq::acc)
  | ' ' | '\t' | '\n' -> tokenize_line stream acc
  | c when is_number c -> 
    tokenize_line stream ((lex_number stream (Char.escaped c))::acc)
  | c -> tokenize_line stream ((lex_keyword stream (Char.escaped c))::acc)
  | exception Stream.Failure -> List.rev acc

(* 

let print_char_opt = print_endline "Print char";
function
| None -> print_char '\n'
| Some c -> print_char c; print_newline ()

let print_string_opt =
print_endline "Print string"; 
function
| None -> print_endline "" 
| Some c -> print_endline c


let rec next_char (str_stream : string Stream.t) (char_stream : char Stream.t) =
  char_stream |> Stream.peek |> print_char_opt;
  str_stream |> Stream.peek |> print_string_opt;
  match Stream.next char_stream with
  | c -> c
  | exception Stream.Failure -> print_endline "Moving to next line"; 
    Stream.junk str_stream;
    next_char (str_stream) (stream_of_line str_stream)

*)


(* 
abg
cde
f *)
(* let rec next_char (stream : string Stream.t) =
  match (stream |> sn |> Stream.next) with
  | s -> print_char s; s
  | exception Stream.Failure -> next_char stream *)

  (* in 
  Stream.iter (fun v -> print_endline v) input_stream *)

(* Stream.of_string, which builds a character stream from a string
 *)
