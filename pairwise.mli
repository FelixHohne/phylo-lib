(** [align_pair d1 d2 align misalign indel] is an array of aligned 
    dna sequences [|d1; d2|] using [align], [misalign], [indel].
    The algorithm used is Needleman-Wunsch. *)
val align_pair : Dna.t -> Dna.t -> int -> int -> int -> Dna.t array