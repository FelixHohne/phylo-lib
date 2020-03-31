open OUnit2
open Tree
open Lexer

let file1 = "test.txt"
let s = stream_of_file file1
let t1 = [LAngle; Clade; RAngle; Word "x"; LAngle; Slash; Clade; RAngle]
let t2 = [LAngle; Description; RAngle; Word "contains"; Word "examples";
          Word "of"; Word "commonly"; Word "used"; Word "elements"; LAngle; Slash;
          Description; RAngle]
let t3 = [LAngle; Name; RAngle; Word "Alcohol"; Word "dehydrogenase";
          Word "class-3"; LAngle; Slash; Name; RAngle]
let t4 = [LAngle; Word "accession"; Word "source"; Eq; Quote; Word "UniProtKB";
          Quote; RAngle; Word "P81431"; LAngle; Slash; Word "accession"; RAngle]
let t5 = [LAngle; ID; Word "provider"; Eq; Quote; Word "ncbi"; Quote; RAngle;
          Num 6645; LAngle; Slash; ID; RAngle]

let a1 = tokenize_next_line s
let a2 = tokenize_next_line s
let a3 = tokenize_next_line s
let a4 = tokenize_next_line s
let a5 = tokenize_next_line s

let tokenize_line = [
  "tokenize line 1" >:: (fun _ -> assert_equal t1 a1);
  "tokenize line 2" >:: (fun _ -> assert_equal t2 a2);
  "tokenize line 3" >:: (fun _ -> assert_equal t3 a3);
  "tokenize line 4" >:: (fun _ -> assert_equal t4 a4);
  "tokenize line 5" >:: (fun _ -> assert_equal t5 a5);
]



let tests =
  "test suite for lexer"  >::: List.flatten [
    tokenize_line
  ]

let _ = run_test_tt_main tests

