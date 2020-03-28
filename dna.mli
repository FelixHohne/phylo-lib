(** The abstract data type representing a DNA sequence. *)
type t


(** [from_fasta string] parses a .FASTA file and creates a DNA sequence. 
    Precondition: string input into from_fasta is the absolute file location 
    of a valid .fasta file 
    Implementation Note: Supports DNA sequences only. *)
val from_fasta: string -> t

(**[get t int] is the DNA letter at position [int]. Zero indexed. Returns None
   if [int] is not a valid position for [t]. *)
val get: t -> int -> string option 

(** [is_empty t ] is true iff. t is empty *)
val is_empty: t -> bool 
