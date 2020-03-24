open OUnit2
open Tree


  let dog = add_species empty 0 "dog" 
  let dog_cat = add_species dog 0  "cat" 
  let dog_cat_mouse = add_species dog_cat 0 "mouse" 


let tree_empty = [
  
  "empty_test" >:: (fun _ -> assert_equal true (is_empty(empty)));
]

let tree_add = [

  "zero size" >:: (fun _ -> assert_equal (size empty) 0); 
  "add_test" >:: (fun _ -> assert_equal (size dog_cat_mouse) 4);

]
let tests =
  "test suite for phylo_lib tree"  >::: List.flatten [
    tree_empty;
    tree_add; 
  ]

let _ = run_test_tt_main tests