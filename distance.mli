
(** The representation type for distance matrices. *)
type t

(** An index in the distance matrix *)
type index = int * int

(** [dist_dna dnas align misalign indel] is the distance matrix created from 
    pairwise alignments of the sequences in [dnas], with align, misalign, and 
    indel penalties given by [align], [misalign], and [indel]. *)
val dist_dna : Dna.t array -> int -> int -> int -> t

(** [dist_msa msa gap] is the distance matrix created from [msa]. Distances are 
    based on Hamming distance, where a distance of 1 represents a mismatch 
    between two different nucleotides, and a distance of [gap] when either a gap
    or a nucleotide is compared with a gap. *)
val dist_msa : Msa.t -> int -> t

(** [min_index dist_matrix] is a pair of indices of DNA sequences with the 
    minimum distance between them. *)
val min_index : t -> index

(** [min_diff dist_matrix] is the minimum distance between any two different
    DNA sequences. *)
val min_diff : t -> float

(**[combine i j dist_matrix] is the result of combining sequences [i] and [j], 
   and averaging their distance against every other sequence.
   Requires: [i] < [j]. *)
val combine : int -> int -> t -> t

(**[is_done dist_matrix] is true if there is only 1 sequence in [dist_matrix].*)
val is_done : t -> bool