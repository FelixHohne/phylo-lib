open OUnit2
open Tree
open Sample_trees

let dog = leaf "dog"
let cat = leaf "cat"
let mouse = leaf "mouse"
let dog_cat = zip [dog; cat]
let dog_cat_mouse = zip [dog_cat; mouse]
let z = zip [mouse; dog_cat]

let tree_empty = [
  "empty_test" >:: (fun _ -> assert_equal true (is_empty(empty)));
]

let tree_size = [
  "zero size" >:: (fun _ -> assert_equal (size empty) 0); 
  "add_test" >:: (fun _ -> assert_equal (size dog_cat_mouse) 5);
]

let tree_zip = [
  "leaf zip" >:: (fun _ -> assert_bool "" (is_equal dog_cat_mouse z));
]

let tree_print = [
  (* can't test printing!! ! *)
]


let tests =
  "test suite for phylo_lib tree"  >::: List.flatten [
    tree_empty;
    tree_size; 
    tree_zip;
  ]

let _ = run_test_tt_main tests

