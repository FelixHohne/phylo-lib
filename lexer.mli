(** The type of lexer tokens. *)
type token =
  | Phylogeny | Name | Description
  | Clade | Rank | Confidence
  | Taxonomy | SciName | ID
  | LAngle | Slash | RAngle | Quote | Eq | Num of int | Dot
  | Word of string | True | False

(** [line_stream_of_file f] is a stream of lines from the file with filename 
    [f]. *)
val stream_of_file : string -> string Stream.t

(** [tokenize_next_line stream] is a list of the tokens in [stream].
    Effects: Removes the first element in [stream]. 
    Raises: [EOF] if the end of the file is reached. *)
val tokenize_next_line: string Stream.t -> token list

(** [next_token_builder stream] is a function that takes in a unit and outputs 
    the next token in [stream]. The function returned raises [EOF] when the
    end of the file is reached.

    Sample usage:
    [let x = stream_of_file "file.txt" in
    let next_token = next_token_builder x in
    next_token ()] will output the first token in file "file.txt". *)
val next_token_builder : string Stream.t -> (unit -> token)