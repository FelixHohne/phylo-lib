(** The abstract data type representing a DNA sequence. *)
type t

type dna = A | C | T | G | Gap | Mismatch 

(** [from_fasta string] parses a .FASTA file and creates a DNA sequence. 
    Precondition: string input into from_fasta is the absolute file location 
    of a valid .fasta file 
    Implementation Note: Supports DNA only. Other characters ignored. *)
val from_fasta : string -> t

(** [from_fastas lst] parses .FASTA files in lst and creates an array of DNA sequences. 
 * The ordering of DNA sequences from [lst] is maintained. *)
val from_fastas : string list -> t array 

(** [from_string] parses a .FASTA file and creates a DNA sequence. 
    Note: name will be empty. *)
val from_string : string -> t

(** [is_empty t ] is true iff. t is empty *)
val is_empty : t -> bool 

(** [length t] is the number of base pairs stored in [t]. *)
val length : t -> int

(** [get t int] is the DNA letter at [pos]. 0 indexed. Returns None
    if [int] is not a valid position for [t]. *)
val get_opt : t -> int -> char option 

(** [get_e dna pos] is the DNA letter at position [pos].0 indexed.
    Requires [pos] is a valid position. *)
val get_e: t -> int -> char 

(** [get_name t] is the name of DNA sequence [t]. *)
val get_name: t -> string 

(** [extract_names dnas] is a string array of the names in dnas *)
val extract_names: t array -> string array

(** [string_of_range t start finish] is a string with base pairs represented as 
    chars in the interval from [t.start] to [t.finish], excluding [t.finish].
    Requires: [start], [finish] are valid positions in [t], [finish] > [start]. 
    Performance: O(n). *)
val string_of_range : t -> int -> int -> string

(** [to_string t] is a string representation of dna sequence [t]. *)
val to_string: t -> string 
 
