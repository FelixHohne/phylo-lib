
(** [upgma dist_matrix species] is a phylogenetic tree produced from 
    [dist_matrix] with species named [species] using the unweighted pair 
    group method with arithmetic mean (UPGMA) algorithm. 
    Effects: Modifies [dist_matrix]. *)
val upgma : Distance.t -> string array -> Tree.t