(* Credit: CS 3110 SP 2015.. Algorithm by: 
https://www.lri.fr/~filliatr/ftp/publis/puf-wml07.ps 
Not written by us. *) 

module type fun_array = sig 

  (** The parameterized abstract type of fun_array *)
  type 'a t 

  (** [make n init_value] creates a functional array of length [n]
      with initial starting value of [init_value] in every position of array *)
  val make : int -> 'a -> 'a t 

(** [get arr n] is element [n] in functional array [arr] *)
  val get : 'a t -> int -> 'a 


(** [set arr n new_val] is the functional array [arr] with [new_val] inserted 
    at position [n] *)
  val set : 'a t -> int -> 'a -> 'a t 
