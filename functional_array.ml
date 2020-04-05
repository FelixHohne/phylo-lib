(* Credit: CS 3110 SP 2015. Algorithm by: 
https://www.lri.fr/~filliatr/ftp/publis/puf-wml07.ps 
Not written by us. *) 

type 'a t = 'a data ref
and 'a data = Arr of 'a array | Diff of int * 'a * 'a t

let make (n : int) (x : 'a) : 'a t =
  ref (Arr (Array.make n x))

(* Effects: reverses the list of Diff nodes along the path to the Arr *)
let rec reroot (a : 'a t) : unit =
  match !a with
      Arr _ -> ()
    | Diff(i, x, a') ->
        reroot a';
        match !a' with
            Diff _ -> failwith "impossible"
          | Arr(arr) ->
              a := Arr(arr);
              a' := Diff(i, arr.(i), a);
              arr.(i) <- x
                  
let rec get (a : 'a t) (i : int) : 'a =
  reroot a;
  match !a with
      Diff(j, x, a') -> failwith "impossible"
    | Arr arr -> arr.(i)

let rec set (a : 'a t) (i : int) (x : 'a) : 'a t =
  reroot a;
  match !a with
      Arr arr ->
        let a' = ref (Arr arr) in
          a := Diff(i, arr.(i), a');
          arr.(i) <- x;
          a'
    | Diff _ -> ref (Diff(i, x, a))

