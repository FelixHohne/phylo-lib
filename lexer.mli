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