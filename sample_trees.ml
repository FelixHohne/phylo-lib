open Tree

module SampleTrees = struct
  let dog_cat_mouse = zip_no_params [leaf_no_params "dog"; 
                                     leaf_no_params "cat"; 
                                     leaf_no_params "mouse"]
  let s1 = leaf_no_params "a"
  let s2 = leaf_no_params "b"
  let s3 = leaf_no_params "c"
  let s4 = leaf_no_params "d"
  let z = zip_no_params [s1; s2]
  let h = zip_no_params [leaf_no_params "c"; leaf_no_params "d"]
  let l = zip_no_params [leaf_no_params "e"; leaf_no_params "h"]
  let i = zip_no_params [leaf_no_params "f";l]
  let j = zip_no_params [h; leaf_no_params "b"; i]

  let single_clade = zip_no_params [s1; s2; s3; s4]
  let multi_1 = zip_no_params [z;dog_cat_mouse]
  let multi_2 = zip_no_params [leaf_no_params "a"; j; leaf_no_params "g"]
end