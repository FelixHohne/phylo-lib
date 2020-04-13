(** The abstract data type representing a DNA sequence. *)
type t

type dna = A | C | T | G | Gap | Mismatch 

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
val get : t -> int -> char option 

(** [get_name t] is the name of DNA sequence [t]. *)
val get_name: t -> string 

(** [extract_names dnas] is a string list of the names in dnas *)
val extract_names: t list -> string list

(** [string_of_range t start finish] is a string with base pairs represented as 
    chars in the interval from [t.start] to [t.finish], excluding [t.finish].
    Requires: [start], [finish] are valid positions in [t], [finish] > [start]. 
    Performance: O(n). *)
val string_of_range : t -> int -> int -> string

(** [to_string t] is a string representation of dna sequence [t]. *)
val to_string: t -> string 

(** [append dna dna_seq] appends [dna] to the end of [dna_seq]. 
    Performance: O(1). *)
val append: dna -> t -> unit 

(** [mutate d pos dna_seq] replaces previous dna sequence at [pos] with 
    [d] in [dna_seq]. 
    Requires: [pos] is a valid index in [dna_seq]. 
    Performance: O(1). *)
val mutate: dna -> int -> t -> unit 

(** [insert d pos dna_seq] inserts [d] at [pos] in [dna_seq]. 
    The previous value at [pos] and all elements to the right are shifted 
    to the right by 1. 
    Example: [A; T; C; G] -> insert A 1 -> [A; A; T; C; G]
    Performance: O(n).  
    Requires: pos is a valid index in [dna_seq]. To add a nucleotide 
    to the end of a [dna_seq], use append. *)
val insert: dna -> int -> t -> unit

