(** pairwise.ml computes pairwise alignments of two DNA sequences. *)

(** [align_pair d1 d2 align misalign indel] is a pair where the first element 
    is an array of aligned dna sequences [|d1; d2|] using [align], [misalign], 
    [indel]. The algorithm used is Needleman-Wunsch. The second element is the
    alignment score. *)
val align_pair : Dna.t -> Dna.t -> int -> int -> int -> (Dna.t array * int)

(** [diff d1 d2 gap] is the difference score obtained after
    aligning [d1] and [d2]. *)
val diff :  Dna.t -> Dna.t -> int -> int -> int -> int