open OUnit2
open Tree
open Lexer

let file1 = "resources/test.txt"
let file2 = "resources/test2.txt"
let s = stream_of_file file1
let t1 = [LAngle; Clade; RAngle; Word "x"; LAngleSlash; Clade; RAngle]
let t2 = [LAngle; Description; RAngle; Word "contains"; Word "examples";
          Word "of"; Word "commonly"; Word "used"; Word "elements"; LAngleSlash;
          Description; RAngle]
let t3 = [LAngle; Name; RAngle; Word "Alcohol"; Word "dehydrogenase";
          Word "class-3"; LAngleSlash; Name; RAngle]
let t4 = [LAngle; Word "accession"; Word "source"; Eq; Quote; Word "UniProtKB";
          Quote; RAngle; Word "P81431"; LAngleSlash; Word "accession"; RAngle]
let t5 = [LAngle; ID; Word "provider"; Eq; Quote; Word "ncbi"; Quote; RAngle;
          Num 6645; LAngleSlash; ID; RAngle]

let a1 = tokenize_next_line s
let a2 = tokenize_next_line s
let a3 = tokenize_next_line s
let a4 = tokenize_next_line s
let a5 = tokenize_next_line s

let s2 = stream_of_file file2
let token_fun = token_function_builder s2 
let peek = token_fun true 
let consume = token_fun false 

let consume_token () = 
  ignore (consume ())

let n1 = peek ()
let () = consume_token ()
let n2 = peek ()
let () = consume_token ()
let n3 = peek ()
let () = consume_token ()
let n4 = peek ()
let () = consume_token ()
let n5 = peek ()
let () = consume_token ()
let () = consume () |> ignore; consume () |> ignore;
  consume () |> ignore; ()
let n6 = peek ()

let tokenize_line = [
  "tokenize line 1" >:: (fun _ -> assert_equal t1 a1);
  "tokenize line 2" >:: (fun _ -> assert_equal t2 a2);
  "tokenize line 3" >:: (fun _ -> assert_equal t3 a3);
  "tokenize line 4" >:: (fun _ -> assert_equal t4 a4);
  "tokenize line 5" >:: (fun _ -> assert_equal t5 a5);
]

let next_token = [
  "next token 1" >:: (fun _ -> assert_equal LAngle n1);
  "next token 2" >:: (fun _ -> assert_equal Clade n2);
  "next token 3" >:: (fun _ -> assert_equal RAngle n3);
  "next token 4" >:: (fun _ -> assert_equal (Word "x") n4);
  "next token 5" >:: (fun _ -> assert_equal LAngleSlash n5);
  "eof" >:: (fun _ -> assert_equal EOF n6);
]

let tests =
  "test suite for lexer"  >::: List.flatten [
    tokenize_line; 
    next_token
  ]

let _ = run_test_tt_main tests

