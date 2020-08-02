open Tree
open Distance

(** [add_species s1 s2 checked unchecked] adds [s1] and [s2] as a new clade in 
    [checked]@[unchecked] (which I will refer to as the tree list) if they are 
    not part of any trees in [checked]@[unchecked]. Otherwise, the species that
    is not in the existing tree list is added to the tree that contains the 
    other species. *)
let add_species species i j acc = 
  let s1 = species.(i) in
  let s2 = species.(j) in
  match (List.assoc_opt i acc), (List.assoc_opt j acc) with
  | None, None -> 
    ((Stdlib.min i j), zip_no_params [leaf_no_params s1; leaf_no_params s2])
    ::acc
  | Some t, None -> let r = List.remove_assoc i acc in 
    ((Stdlib.min i j ), zip_no_params [leaf_no_params s2; t])::r
  | None, Some t -> let r = List.remove_assoc j acc in 
    ((Stdlib.min i j ), zip_no_params [leaf_no_params s1; t])::r
  | Some t1, Some t2 ->
    let r = acc |> List.remove_assoc i |> List.remove_assoc j in 
    ((Stdlib.min i j), zip_no_params [t1; t2])::r

(** [upgma_help dist species acc] is the phylogenetic tree that results from
    running the UPGMA algorithm on [dist] with species names [species] and 
    zipping up the results with [acc].

    Preconditions: if [acc] is empty, then [is_done dist] must be false, i.e. 
    there must be more than one remaining column in distance matrix [dist].
    For every column index in [dist] there must be a corresponding name in 
    [species]. *)
let rec upgma_help dist species acc = 
  if is_done dist then 
    begin
      let tree_list = (List.rev_map (fun x -> snd x) acc) in
      if List.length tree_list = 1 then (List.hd tree_list)
      else if List.length tree_list > 1 then zip_no_params tree_list
      else failwith "Precondition violated in upgma_help"
    end
  else
    begin
      let i, j = min_index dist in
      let t = add_species species i j acc in
      let d = combine i j dist in 
      upgma_help d species t
    end

let upgma dist species = 
  match Array.length species with
  | 0 -> Tree.empty
  | 1 -> Tree.leaf_no_params species.(0)
  | _ -> upgma_help dist species []


let rec factorial_range n l acc  = if n = l then acc*l else 
    factorial_range (n-1) l (acc*n)

(**[b s] is the total number of rooted trees possible with [s] species.*)
let b s = (factorial_range (2*s-3) (s-2) 1) / (1 lsl (s-2))

(**[substitution i j] is the probability of base pair [i] transitioning to [j]*)
let substitution i j = 0.25

(**[uninf_prior t s] is the probability of tree [t] with [s] species.*)
let uninf_prior t s = 1. /. (float_of_int (b s))

let rec initialize species i acc = if i == Array.length species then acc else
    begin
      let leaf = i |> Array.get species |> leaf_no_params in
      if Tree.is_empty acc then leaf
      else initialize species (i+1) (zip_no_params [leaf; acc])
    end

let log_likelihood_site tree msa species i = 0.5

let rec log_likelihood tree msa species acc finished = 
  if finished = Array.length species then acc else
    log_likelihood tree msa species 
      (acc +. log_likelihood_site tree msa species finished) (finished + 1)

let log_proposal tree1 tree2 = 0.5

let min i j = if i < j then i else j

let max i j = if i > j then i else j

let rec two_random species n =
  let i = Random.int n in
  let j = Random.int n in 
  if species.(i) = species.(j) then two_random species n
  else 
    let ct = if species.(i) = i then 1 else 0 in
    (species.(i), species.(j), if species.(j) = j then ct+1 else ct)

let combine_species i j clubbed =
  Array.set clubbed (max i j) (min i j)

let rec next_help ct species clubbed acc = 
  if ct = Array.length species then 
    begin
      let tree_list = (List.rev_map (fun x -> snd x) acc) in
      if List.length tree_list = 1 then (List.hd tree_list)
      else if List.length tree_list > 1 then zip_no_params tree_list
      else failwith "Precondition violated in next_help"
    end
  else
    begin
      let i, j, new_ct = species |> Array.length |> two_random clubbed in
      let t = add_species species i j acc in
      combine_species i j clubbed;
      next_help new_ct species clubbed t
    end


(** Returns a random tree with the given [species]*)
let next species = 
  let blank = Array.make (Array.length species) 0 in
  let clubbed = Array.mapi (fun i x -> i) blank in
  next_help 0 species clubbed []

let r tree1 tree2 msa species = min 0. (log_likelihood tree2 msa species 0. 0) 
                                -. (log_likelihood tree1 msa species 0. 0) 
                                +. (log_proposal tree1 tree2) 
                                -. (log_proposal tree2 tree1)

let naccept = ref 0
let nreject = ref 0

type chain_state = {tree: Tree.t; chain: Tree.t list}

let mcmc_sampler msa species =
  (* naccept:= 0; nreject:=0; *)
  (fun chain_state ->
     let tree1 = chain_state.tree in
     let tree2 = next species in
     let ratio = r tree1 tree2 msa species in 
     if ratio > 0. then 
       {tree= tree2; chain = tree2::chain_state.chain}
     else if Random.float 1.0 < ratio then
       {tree= tree2; chain = tree2::chain_state.chain}
     else {tree= tree1; chain = tree1::chain_state.chain})

let rec chain_helper sampler state acc = 
  if acc = 100 then state 
  else chain_helper sampler (sampler state) (acc + 1)

let rec fold mode prev max_freq curr_freq l =
  match l with
    [] -> mode
  | x :: xs -> if x = prev then fold mode prev max_freq (curr_freq + 1) xs else
    if curr_freq > max_freq then fold prev x curr_freq 1 xs
    else fold mode x max_freq 1 xs

let bayes msa species =
  let start_tree = initialize species 0 Tree.empty in
  let init = {tree = start_tree; chain = [start_tree]} in
  let sampler = mcmc_sampler msa species in 
  let final = chain_helper sampler init 0 in
  let sorted = List.sort compare final.chain in
  fold start_tree start_tree 0 0 sorted