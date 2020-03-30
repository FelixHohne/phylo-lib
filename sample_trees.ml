open Tree

module SampleTrees = struct
let dog_cat_mouse = zip [leaf "dog"; leaf "cat"; leaf "mouse"]
let s1 = leaf "a"
let s2 = leaf "b"
let s3 = leaf "c"
let s4 = leaf "d"
let z = zip [s1; s2]
let h = zip [leaf "c"; leaf "d"]
let l = zip [leaf "e"; leaf "h"]
let i = zip [leaf "f";l]
let j = zip [h; leaf "b"; i]

let single_clade = zip [s1; s2; s3; s4]
let multi_1 = zip [z;dog_cat_mouse]
let multi_2 = zip [leaf "a"; j; leaf "g"]
end