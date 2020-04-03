type token =
  | Phylogeny | Name | Description
  | Clade | Rank | Confidence
  | Taxonomy | SciName | ID
  | LAngle | LAngleSlash | RAngle | Quote | Eq | Num of int | Dot
  | Word of string | True | False
  | EOF | Unit

type t = bool -> token 

(** Map binding strings to their corresponding token. *)
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

(** [is_token s] is true if [s] represents a valid token. *)
let is_token (s : string) : bool =
  Hashtbl.mem word_token_map s

(** [string_to_token s] converts [s] into a token. 
    Raises: [Not_found] if [is_token s] is false. *)
let string_to_token (s : string) : token =
  Hashtbl.find word_token_map s

let stream_of_file (f : string) : string Stream.t =
  let in_channel = open_in f in 
  Stream.from (fun _ ->
      try Some (input_line in_channel) with End_of_file -> None)

(** [stream_of_line stream] is a character stream of the next line of 
    string stream [stream]. 
    Effects: Removes the first element in [stream]. 
    Raises: [EOF] if the end of the file is reached. *)
let stream_of_line (stream : string Stream.t) : char Stream.t = 
  match (Stream.next stream) with
  | str -> Stream.of_string str
  | exception Stream.Failure -> raise End_of_file

(** [peek_stream_of_line stream] is a character stream of the next line of
    string stream [stream]. Does not modify [stream]. 
    Raises: [EOF] if the end of the file is reached. *)
let peek_stream_of_line (stream : string Stream.t) : char Stream.t = 
  match (Stream.peek stream) with
  | Some str -> Stream.of_string str
  | None -> raise End_of_file

(** [is_special_char c] is true if [c] is a special character. Special 
    characters are: '<', '>', '"', and '='. *)
let is_special_char (c : char) : bool =
  match c with 
  | '<' 
  | '>' 
  | '"'
  | '='-> true
  | _ -> false

(** [lex_keyword stream acc] lexes a word in [stream], taking into account
    the characters that have already been read, which are in [acc]. *)
let rec lex_keyword (stream : char Stream.t) (acc : string) : token =
  match Stream.peek stream with 
  | Some t ->
    begin
      match t with
      | ' ' 
      | exception Stream.Failure -> lex_keyword_helper acc
      | c -> 
        if is_token acc
        then string_to_token acc
        else if (is_special_char c) then Word acc
        else (Stream.junk stream; lex_keyword stream (acc ^ (Char.escaped c)))
    end
  | None -> lex_keyword_helper acc
and 
  lex_keyword_helper (acc : string) : token =
  if is_token acc
  then string_to_token acc else Word acc

(** [is_number c] is true if c represents a numerical digit. *)
let is_number (c : char) : bool = 
  match c with 
  | '0'..'9' -> true
  | _ -> false

(** [lex_number stream acc] lexes a number in [stream], taking into account 
    the digits of the number that have already been read, which are in [acc]. *)
let rec lex_number (stream : char Stream.t) (acc : string) : token =
  let x = Stream.peek stream in
  match x with 
  | Some c when (is_number c) -> 
    Stream.junk stream; 
    lex_number (stream) (acc ^ (Char.escaped c))
  | Some _ | None -> Num (int_of_string acc)

(** [tokenize_line stream] is a list of the tokens in [stream] *)
let rec tokenize_line (stream : char Stream.t) (acc : token list): token list = 
  match Stream.next stream with 
  | '<' -> begin
      match Stream.peek stream with
      | Some n when (n = '/') -> Stream.junk stream; 
        tokenize_line stream (LAngleSlash::acc) 
      | Some n -> tokenize_line stream (LAngle::acc)
      | None -> List.rev (LAngle::acc)
    end
  | '>' -> tokenize_line stream (RAngle::acc)
  | '"' -> tokenize_line stream (Quote::acc)
  | '=' -> tokenize_line stream (Eq::acc)
  | ' ' | '\t' | '\n' | '\r'-> tokenize_line stream acc
  | c when is_number c -> 
    tokenize_line stream ((lex_number stream (Char.escaped c))::acc)
  | c -> tokenize_line stream ((lex_keyword stream (Char.escaped c))::acc)
  | exception Stream.Failure -> List.rev acc
  | exception End_of_file -> [EOF]

(* * Requires: f is either stream_of_line or peak_stream_of
   let tokenize_next_line (stream : string Stream.t)
    (f : (string Stream.t -> char Stream.t)) : token list =
   match f stream with
   | exception End_of_file -> [EOF]
   | x -> tokenize_line x [] *)

let tokenize_next_line (stream : string Stream.t) : token list =
  match stream_of_line stream with
  | exception End_of_file -> [EOF]
  | x -> tokenize_line x []


let token_function_builder (stream : string Stream.t) : (bool -> (unit -> token)) =
  let tokens_in_line = ref (tokenize_next_line stream) in 
  let token_function = ref (fun x -> ( fun () -> EOF)) in
  (token_function := (fun x ->
       if x then (fun () ->
           match !tokens_in_line with
           | [] -> tokens_in_line := (tokenize_next_line stream); 
             !token_function x ()
           | h::_ -> h)
       else (fun () ->
           match !tokens_in_line with
           | [] -> tokens_in_line := (tokenize_next_line stream); Unit
           | _::t -> tokens_in_line := t; Unit))); !token_function


(* let consume_token_builder (stream : string Stream.t) : (bool -> token) =
   let tokens_in_line = ref (tokenize_next_line stream stream_of_line) in 
   let consume_token_fun = ref (fun x -> EOF) in
   (consume_token_fun := (fun x ->
       if x then
         match !tokens_in_line with
         | [] -> tokens_in_line := (tokenize_next_line stream peek_stream_of_line); !consume_token_fun x
         | h::_ -> h
       else 
         match !tokens_in_line with
         | [] -> tokens_in_line := (tokenize_next_line stream stream_of_line); Unit(*!consume_token_fun false*)
         | _::t -> tokens_in_line := t; Unit));
   !consume_token_fun *)


(* 
let peek_token_builder (stream: string Stream.t) = 
  let tokens_in_line = ref (tokenize_next_line stream peek_stream_of_line) in 
  let peek_token_fun = ref (fun () -> EOF) in
  (peek_token_fun := (fun () ->
       begin
         match !tokens_in_line with
         | [] -> tokens_in_line := (tokenize_next_line stream peek_stream_of_line); !peek_token_fun ()
         | h::_ -> h
       end ));
  !peek_token_fun *)

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