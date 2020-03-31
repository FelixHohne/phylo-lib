(** The abstract data type representing a DNA sequence. *)
type t

(** [from_fasta string] parses a .FASTA file and creates a DNA sequence. 
    Precondition: string input into from_fasta is the absolute file location 
    of a valid .fasta file 
    Implementation Note: Supports DNA only. Other characters ignored. *)
val from_fasta : string -> t

(** [is_empty t ] is true iff. t is empty *)
val is_empty : t -> bool 

(** [length t] is the number of base pairs stored in [t]. *)
val length : t -> int

(** [get t int] is the DNA letter at position [int]. 0 indexed. Returns None
    if [int] is not a valid position for [t]. *)
val get : t -> int -> string option 

(** [string_of_range t start finish] is a string with base pairs represented as 
    chars in the interval from [t.start] to [t.finish], excluding [t.finish].
    Requires: [start], [finish] are valid positions in [t], [finish] > [start]. 
    Performance: O(n). *)
val string_of_range : t -> int -> int -> string

(** [phys_equals t t] is the physical equality test for DNA sequences. *)
(* val phys_equals : t -> t -> bool *) 

