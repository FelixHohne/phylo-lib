open OUnit2
open Tree


let tree_empty = [
  
  "empty" >:: (fun _ -> assert_equal true (is_empty(empty)));
  assert(false)

]

let tests =
  "test suite for phylo_lib tree"  >::: List.flatten [
    tree_empty;
  ]

let _ = run_test_tt_main tests