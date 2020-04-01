(** The type of lexer tokens. *)
type token =
  | Phylogeny | Name | Description
  | Clade | Rank | Confidence
  | Taxonomy | SciName | ID
  | LAngle | Slash | RAngle | Quote | Eq | Num of int | Dot
  | Word of string | True | False
  | EOF | Unit

type t = bool -> token

(** [line_stream_of_file f] is a stream of lines from the file with filename 
    [f]. Requires [f] to be a valid file.  *)
val stream_of_file : string -> string Stream.t

(** [tokenize_next_line stream] is a list of tokens in [stream].
    Effects: Removes the first element in [stream]. 
    Requires: f is either stream_of_line or peak_stream_of_line

    Raises: [EOF] if the end of the file is reached. *)
val tokenize_next_line: string Stream.t -> (string Stream.t -> char Stream.t)
  ->  token list 

(** [consume_token_builder stream] is a function that takes in unit and 
    consumes the next token in [stream]. This modifies [stream].

    Sample usage:
    [let x = stream_of_file "file.txt" in
    let consume_token = consume_token_builder x in
    consume_token ()] will consume the first token in file "file.txt". *)
val consume_token_builder : string Stream.t -> (bool -> token)



(* (** [peek_token_builder stream] is a function that takes in unit and 
    returns the next token in [stream], and EOF if [stream] does not have any more tokens. This does not modify [stream].

    Sample usage:
    [let x = stream_of_file "file.txt" in
    let peek_token = peek_token_builder x in
    peek_token ()] will return the first token in file "file.txt". *)
   val peek_token_builder : string Stream.t -> (unit -> token)y *)