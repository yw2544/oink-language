open OUnit2
open Final.Ast
open Final.Main_utils

(**[make_i n i s] makes an OUnit test named [n] that expects [s] to evaluate to
   [i]*)
let make_i n i s = n >:: fun _ -> assert_equal (string_of_int i) (interp s)

let make_s n s1 s2 = n >:: fun _ -> assert_equal s1 (interp s2)
let print_interp input = Printf.printf "Result: %s\n" (interp input)

let all_tests =
  [
    make_i "process integer" 22 "22";
    make_s "process string" "hello world" "\"hello world\"";
    ( "test eval" >:: fun _ ->
      let result = interp "oink x = 3 mud x" in
      assert_equal "3" result );
    ( "test eval" >:: fun _ ->
      let result = interp "oink x = oink y = 4 mud y mud x" in
      assert_equal "4" result );
    ( "test parse" >:: fun _ ->
      assert_equal (Oink ("x", Int 3, Int 3)) (parse "oink x = 3 mud 3") );
    ( "test parse" >:: fun _ ->
      assert_equal (Oink ("x", Int 3, Ident "x")) (parse "oink x = 3 mud x") );
    ( "test parse int" >:: fun _ ->
      let result = parse "5" in
      assert_equal (Int 5) result );
    ( "test parse negative int" >:: fun _ ->
      let result = parse "-9" in
      assert_equal (Int (-9)) result );
    ( "test parse float" >:: fun _ ->
      let result = parse "3.14" in
      assert_equal (Float 3.14) result );
    ( "test parse negative float" >:: fun _ ->
      let result = parse "-2.71" in
      assert_equal (Float (-2.71)) result );
    (* Math operation tests *)
    ( "test PigPile but with +" >:: fun _ ->
      let result = interp "3 + 5" in
      assert_equal "8" result ~printer:(fun x -> x) );
    ( "test PigPile float and int addition" >:: fun _ ->
      let result = interp "3.5 pigpile 2" in
      assert_equal "5.5" result ~printer:(fun x -> x) );
    ( "test PigPile int and float addition" >:: fun _ ->
      let result = interp "3 pigpile 2.5" in
      assert_equal "5.5" result ~printer:(fun x -> x) );
    ( "test SnoutOut" >:: fun _ ->
      let result = interp "10 snoutout 4" in
      assert_equal "6" result ~printer:(fun x -> x) );
    ( "test SnoutOut but with -" >:: fun _ ->
      let result = interp "10 - 4" in
      assert_equal "6" result ~printer:(fun x -> x) );
    ( "test SnoutOut int minus float" >:: fun _ ->
      let result = interp "5 snoutout 2.5" in
      assert_equal "2.5" result ~printer:(fun x -> x) );
    ( "test SnoutOut float minus int" >:: fun _ ->
      let result = interp "5.5 snoutout 2" in
      assert_equal "3.5" result ~printer:(fun x -> x) );
    ( "test MudMultiply" >:: fun _ ->
      let result = interp "6 mudmultiply 7" in
      assert_equal "42" result ~printer:(fun x -> x) );
    ( "test MudMultiply but with *" >:: fun _ ->
      let result = interp "6 * 7" in
      assert_equal "42" result ~printer:(fun x -> x) );
    ( "test MudMultiply float and float multiplication" >:: fun _ ->
      let result = interp "3.0 mudmultiply 2.0" in
      assert_equal "6.0" result ~printer:(fun x -> x) );
    ( "test MudMultiply int and float multiplication" >:: fun _ ->
      let result = interp "3 mudmultiply 2.5" in
      assert_equal "7.5" result ~printer:(fun x -> x) );
    ( "test TroughSplit" >:: fun _ ->
      let result = interp "20 troughsplit 4" in
      assert_equal "5" result ~printer:(fun x -> x) );
    ( "test TroughSplit float and float division" >:: fun _ ->
      let result = interp "6.0 troughsplit 2.0" in
      assert_equal "3.0" result ~printer:(fun x -> x) );
    ( "test TroughSplit int and float division" >:: fun _ ->
      let result = interp "6 troughsplit 2.5" in
      assert_equal "2.4" result ~printer:(fun x -> x) );
    ( "test TroughSplit float division by zero" >:: fun _ ->
      assert_raises Division_by_zero (fun () ->
          ignore (interp "6.0 troughsplit 0.0")) );
    ( "test PigPile with float" >:: fun _ ->
      let result = interp "3.5 pigpile 2.5" in
      assert_equal "6.0" result ~printer:(fun x -> x) );
    ( "test SnoutOut with float" >:: fun _ ->
      let result = interp "7.8 snoutout 2.3" in
      assert_equal "5.5" result ~printer:(fun x -> x) );
    ( "test MudMultiply with float" >:: fun _ ->
      let result = interp "2.5 mudmultiply 4" in
      assert_equal "10.0" result ~printer:(fun x -> x) );
    ( "test arithmetic with parentheses" >:: fun _ ->
      let result = interp "(1+2)*3" in
      assert_equal "9" result ~printer:(fun x -> x) );
    ( "test Oink assignment with parenthesis and unused variable" >:: fun _ ->
      let result = interp "oink x = (3+1) mud 2" in
      assert_equal "2" result ~printer:(fun x -> x) );
    ( "test Oink assignment with parenthesis and used variable" >:: fun _ ->
      let result = interp "oink x = (3+1) mud x+1" in
      assert_equal "5" result ~printer:(fun x -> x) );
    ( "test Oink assignment with parenthesis in both part and used variable"
    >:: fun _ ->
      let result = interp "oink x = (3+1) mud x+(1+2)*3" in
      assert_equal "13" result ~printer:(fun x -> x) );
    ( "test string concatenation with Oink" >:: fun _ ->
      let result = interp "oink y = \"good \" mud y ^ \"job\"" in
      assert_equal "good job" result ~printer:(fun x -> x) );
    ( "test TroughSplit with float" >:: fun _ ->
      let result = interp "15.0 troughsplit 3" in
      assert_equal "5.0" result ~printer:(fun x -> x) );
    ( "test TroughSplit division by zero" >:: fun _ ->
      try
        let _ = interp "10 troughsplit 0" in
        assert_failure "Expected division by zero error"
      with
      | Failure msg -> assert_equal "Error: Division by zero." msg
      | Division_by_zero -> ()
      | e ->
          let error_msg = Printexc.to_string e in
          assert_failure ("Unexpected exception raised: " ^ error_msg) );
    ( "test PigPile mixed types" >:: fun _ ->
      let result = interp "3 pigpile 2.5" in
      assert_equal "5.5" result ~printer:(fun x -> x) );
    ( "test SnoutOut with first value and second pending evaluation" >:: fun _ ->
      let result = interp "10 snoutout (5 + 3)" in
      assert_equal "2" result ~printer:(fun x -> x) );
    ( "test SnoutOut with nested expressions" >:: fun _ ->
      let result = interp "15 snoutout (7 snoutout 2)" in
      assert_equal "10" result ~printer:(fun x -> x) );
    ( "test SnoutOut with first value and delayed computation" >:: fun _ ->
      let result = interp "20 snoutout (10 * 2 troughsplit 4)" in
      assert_equal "15" result ~printer:(fun x -> x) );
    ( "test oink no mud" >:: fun _ ->
      let result = interp "oink x = 3;" in
      assert_equal "Squeal" result ~printer:(fun x -> x) );
    ( "test oink no mud evaluated" >:: fun _ ->
      let result = interp "oink x = 11;" in
      let result2 = interp "x" in
      assert_equal "11" result2 ~printer:(fun x -> x);
      assert_equal "Squeal" result ~printer:(fun x -> x) );
    ( "test oink no mud" >:: fun _ ->
      let result = interp "oink x = 3;" in
      assert_equal "Squeal" result ~printer:(fun x -> x) );
    ( "test oink no mud evaluated" >:: fun _ ->
      let result = interp "oink x = 11;" in
      let result2 = interp "x" in
      assert_equal "11" result2 ~printer:(fun x -> x);
      assert_equal "Squeal" result ~printer:(fun x -> x) );
    (* And tests*)
    ( "test And with both true" >:: fun _ ->
      let result = interp "true and true" in
      assert_equal "true" result ~printer:(fun x -> x) );
    ( "test And with one false" >:: fun _ ->
      let result = interp "true and false" in
      assert_equal "false" result ~printer:(fun x -> x) );
    ( "test And with both false" >:: fun _ ->
      let result = interp "false and false" in
      assert_equal "false" result ~printer:(fun x -> x) );
    ( "test And with e1 true, e2 requires evaluation" >:: fun _ ->
      let result = interp "true and (1 eq 1)" in
      assert_equal "true" result ~printer:(fun x -> x) );
    ( "test And with e1 false, e2 not evaluated" >:: fun _ ->
      let result = interp "false and (1 eq 1)" in
      assert_equal "false" result ~printer:(fun x -> x) );
    ( "test And with e1 and e2 requiring steps" >:: fun _ ->
      let result = interp "(2 eq 2) and (3 eq 3)" in
      assert_equal "true" result ~printer:(fun x -> x) );
    (* Or tests *)
    ( "test Or with both true" >:: fun _ ->
      let result = interp "true or true" in
      assert_equal "true" result ~printer:(fun x -> x) );
    ( "test Or with one true" >:: fun _ ->
      let result = interp "false or true" in
      assert_equal "true" result ~printer:(fun x -> x) );
    ( "test Or with both false" >:: fun _ ->
      let result = interp "false or false" in
      assert_equal "false" result ~printer:(fun x -> x) );
    ( "test And with both true" >:: fun _ ->
      let result = interp "true and true" in
      assert_equal "true" result ~printer:(fun x -> x) );
    ( "test And with one false" >:: fun _ ->
      let result = interp "true and false" in
      assert_equal "false" result ~printer:(fun x -> x) );
    ( "test And with both false" >:: fun _ ->
      let result = interp "false and false" in
      assert_equal "false" result ~printer:(fun x -> x) );
    ( "test Or with both true" >:: fun _ ->
      let result = interp "true or true" in
      assert_equal "true" result ~printer:(fun x -> x) );
    ( "test Or with one true" >:: fun _ ->
      let result = interp "false or true" in
      assert_equal "true" result ~printer:(fun x -> x) );
    ( "test Or with both false" >:: fun _ ->
      let result = interp "false or false" in
      assert_equal "false" result ~printer:(fun x -> x) );
    ( "test Or with e1 true, e2 not evaluated" >:: fun _ ->
      let result = interp "true or (1 eq 1)" in
      assert_equal "true" result ~printer:(fun x -> x) );
    ( "test Or with e1 false, e2 requires evaluation" >:: fun _ ->
      let result = interp "false or (1 eq 1)" in
      assert_equal "true" result ~printer:(fun x -> x) );
    ( "test Or with e1 and e2 requiring steps" >:: fun _ ->
      let result = interp "(1 eq 2) or (3 eq 3)" in
      assert_equal "true" result ~printer:(fun x -> x) );
    (* Float equality *)
    ( "test EQ with equal floats" >:: fun _ ->
      let result = interp "3.14 eq 3.14" in
      assert_equal "true" result ~printer:(fun x -> x) );
    ( "test EQ with unequal floats" >:: fun _ ->
      let result = interp "2.71 eq 3.14" in
      assert_equal "false" result ~printer:(fun x -> x) );
    ( "test EQ with very close floats" >:: fun _ ->
      let result = interp "1.0000001 eq 1.0000001" in
      assert_equal "true" result ~printer:(fun x -> x) );
    (* String equality *)
    ( "test EQ with equal strings" >:: fun _ ->
      let result = interp "\"hello\" eq \"hello\"" in
      assert_equal "true" result ~printer:(fun x -> x) );
    ( "test EQ with unequal strings" >:: fun _ ->
      let result = interp "\"hello\" eq \"world\"" in
      assert_equal "false" result ~printer:(fun x -> x) );
    ( "test EQ with case-sensitive strings" >:: fun _ ->
      let result = interp "\"Hello\" eq \"hello\"" in
      assert_equal "false" result ~printer:(fun x -> x) );
    (* Boolean equality *)
    ( "test EQ with true and true" >:: fun _ ->
      let result = interp "true eq true" in
      assert_equal "true" result ~printer:(fun x -> x) );
    ( "test EQ with false and false" >:: fun _ ->
      let result = interp "false eq false" in
      assert_equal "true" result ~printer:(fun x -> x) );
    ( "test EQ with true and false" >:: fun _ ->
      let result = interp "true eq false" in
      assert_equal "false" result ~printer:(fun x -> x) );
    ( "test workhorse" >:: fun _ ->
      let result = interp "workhorse example x #x baaa x#" in
      assert_equal "Squeal" result ~printer:(fun x -> x) );
    ( "test workhorse evaluated return expr is value" >:: fun _ ->
      let result = interp "workhorse example x #6 baaa 3#" in
      let evaluated = interp "go! example 3" in
      assert_equal "Squeal" result ~printer:(fun x -> x);
      assert_equal "3" evaluated ~printer:(fun x -> x) );
    ( "test workhorse evaluated return expr is not value" >:: fun _ ->
      let result = interp "workhorse example x # baaa x#" in
      let evaluated = interp "go! example 3" in
      assert_equal "Squeal" result ~printer:(fun x -> x);
      assert_equal "3" evaluated ~printer:(fun x -> x) );
    ( "test workhorse containing oink no mud" >:: fun _ ->
      let result = interp "workhorse example x # oink y = 4; baaa y#" in
      let evaluated = interp "go! example 3" in
      assert_equal "Squeal" result ~printer:(fun x -> x);
      assert_equal "4" evaluated ~printer:(fun x -> x) );
    ( "test workhorse nested" >:: fun _ ->
      let result =
        interp
          "workhorse example x # workhorse exp y #4 baaa y# baaa go! exp 4#"
      in
      let evaluated = interp "go! example 3" in
      assert_equal "Squeal" result ~printer:(fun x -> x);
      assert_equal "4" evaluated ~printer:(fun x -> x) );
    ( "test workhorse alternate definition. oink + anonymous function"
    >:: fun _ ->
      let result = interp "oink test = workhorse x # oink y = 14; baaa y#;" in
      let evaluated = interp "go! test 3" in
      assert_equal "Squeal" result ~printer:(fun x -> x);
      assert_equal "14" evaluated ~printer:(fun x -> x) );
    ( "test workhorse multi input 1st input" >:: fun _ ->
      let func_def = interp "workhorse example [a; b] #baaa a#" in
      let evaluated = interp "go! example [3; 5]" in
      assert_equal "Squeal" func_def ~printer:(fun x -> x);
      assert_equal "3" evaluated ~printer:(fun x -> x) );
    ( "test workhorse multi input 2nd input" >:: fun _ ->
      let func_def = interp "workhorse example [a; b] #baaa b#" in
      let evaluated = interp "go! example [3; 5]" in
      assert_equal "Squeal" func_def ~printer:(fun x -> x);
      assert_equal "5" evaluated ~printer:(fun x -> x) );
    ( "test lexical scope of functions" >:: fun _ ->
      let _ = interp "oink x = 54;" in
      let func_def = interp "workhorse example [a; b] #baaa x#" in
      let _ = interp "oink x = 88;" in
      let evaluated = interp "go! example [3; 5]" in
      assert_equal "Squeal" func_def ~printer:(fun x -> x);
      assert_equal "54" evaluated ~printer:(fun x -> x) );
    ( "test scope of function" >:: fun _ ->
      let _ = interp "oink x = 54;" in
      let func_def = interp "workhorse example [x; b] #baaa x#" in
      let _ = interp "oink x = 88;" in
      let evaluated = interp "go! example [3; 5]" in
      assert_equal "Squeal" func_def ~printer:(fun x -> x);
      assert_equal "3" evaluated ~printer:(fun x -> x) );
    (* Pen operations test*)
    (* PenPen - Append two Pen lists *)
    ( "PenPen append two Pen lists" >:: fun _ ->
      let result = interp "[1; 2] penpen [3; 4]" in
      assert_equal "pen: [1, 2, 3, 4]" result );
    (* PenPen - Append a Pen list to empty Pen list *)
    ( "PenPen append to empty list" >:: fun _ ->
      let result = interp "[] penpen [3; 4]" in
      assert_equal "pen: [3, 4]" result );
    (* PenPen - Append empty Pen list to another Pen list *)
    ( "PenPen append empty list to Pen list" >:: fun _ ->
      let result = interp "[1; 2] penpen []" in
      assert_equal "pen: [1, 2]" result );
    (* PenPen - Append two empty Pen lists *)
    ( "PenPen append two empty lists" >:: fun _ ->
      let result = interp "[] penpen []" in
      assert_equal "pen: []" result );
    (* PenPen - Nested Pen lists *)
    ( "PenPen append nested Pen lists" >:: fun _ ->
      let result = interp "[[1; 2]; [3; 4]] penpen [[5; 6]]" in
      assert_equal "pen: [pen: [1, 2], pen: [3, 4], pen: [5, 6]]" result );
    (* Ppen - Prepend a value to a Pen list *)
    ( "Ppen prepend a value to Pen list" >:: fun _ ->
      let result = interp "1 ppen [2; 3]" in
      assert_equal "pen: [1, 2, 3]" result );
    (* Ppen - Prepend a value to an empty Pen list *)
    ( "Ppen prepend value to empty list" >:: fun _ ->
      let result = interp "1 ppen []" in
      assert_equal "pen: [1]" result );
    (* Ppen - Prepend a value to a Pen list with a single element *)
    ( "Ppen prepend value to single-element list" >:: fun _ ->
      let result = interp "0 ppen [1]" in
      assert_equal "pen: [0, 1]" result );
    (* Ppen - Prepend a value to a Pen list with multiple elements *)
    ( "Ppen prepend value to multi-element Pen list" >:: fun _ ->
      let result = interp "5 ppen [1; 2; 3]" in
      assert_equal "pen: [5, 1, 2, 3]" result );
    (* Ppen - Prepend a value to an already empty Pen list *)
    ( "Ppen prepend to empty list" >:: fun _ ->
      let result = interp "99 ppen []" in
      assert_equal "pen: [99]" result );
    (* Ppen - Prepend a value to a Pen list with elements of different types *)
    ( "Ppen prepend value to list with different types" >:: fun _ ->
      let result = interp "\"hello\" ppen [\"hi\"; \"hii\"; \"hiii\"]" in
      assert_equal "pen: [hello, hi, hii, hiii]" result );
    (* PenSnatch - Remove element from a list by valid index *)
    ( "PenSnatch remove element at index 1" >:: fun _ ->
      let result = interp "[1; 2; 3] pensnatch 1" in
      assert_equal "pen: [1, 3]" result );
    (* PenSnatch - Remove first element *)
    ( "PenSnatch remove first element" >:: fun _ ->
      let result = interp "[10; 20; 30] pensnatch 0" in
      assert_equal "pen: [20, 30]" result );
    (* PenSnatch - Remove last element *)
    ( "PenSnatch remove last element" >:: fun _ ->
      let result = interp "[4; 5; 6] pensnatch 2" in
      assert_equal "pen: [4, 5]" result );
    (* PenSnatch - Remove second element from a list of 4 elements *)
    ( "PenSnatch remove second element" >:: fun _ ->
      let result = interp "[7; 8; 9; 10] pensnatch 1" in
      assert_equal "pen: [7, 9, 10]" result );
    (* PenSnatch - Remove third element from a list of 5 elements *)
    ( "PenSnatch remove third element" >:: fun _ ->
      let result = interp "[5; 6; 7; 8; 9] pensnatch 2" in
      assert_equal "pen: [5, 6, 8, 9]" result );
    (* PenSnatch - Remove middle element from an odd-length list *)
    ( "PenSnatch remove middle element from odd-length list" >:: fun _ ->
      let result = interp "[11; 22; 33; 44; 55] pensnatch 2" in
      assert_equal "pen: [11, 22, 44, 55]" result );
    (* PenSnatch - Remove middle element from an even-length list *)
    ( "PenSnatch remove middle element from even-length list" >:: fun _ ->
      let result = interp "[1; 3; 5; 7; 9; 11] pensnatch 3" in
      assert_equal "pen: [1, 3, 5, 9, 11]" result );
    (* PenSnatch - Remove first element from a list of strings *)
    ( "PenSnatch remove first element from string list" >:: fun _ ->
      let result = interp "[\"a\"; \"b\"; \"c\"; \"d\"] pensnatch 0" in
      assert_equal "pen: [b, c, d]" result );
    (* PenSnatch - Remove second element from a list of strings *)
    ( "PenSnatch remove second element from string list" >:: fun _ ->
      let result = interp "[\"apple\"; \"banana\"; \"cherry\"] pensnatch 1" in
      assert_equal "pen: [apple, cherry]" result );
    (* PenSnatch - Remove element from nested Pen list *)
    ( "PenSnatch remove element from nested Pen list" >:: fun _ ->
      let result = interp "[[1; 2]; [3; 4]; [5; 6]] pensnatch 1" in
      assert_equal "pen: [pen: [1, 2], pen: [5, 6]]" result );
    (* PenSnatch - Remove second element from a longer list *)
    ( "PenSnatch remove second element from longer list" >:: fun _ ->
      let result = interp "[100; 200; 300; 400; 500] pensnatch 1" in
      assert_equal "pen: [100, 300, 400, 500]" result );
    (* PenSnatch - Remove element from list of floats *)
    ( "PenSnatch remove element from float list" >:: fun _ ->
      let result = interp "[1.1; 2.2; 3.3; 4.4] pensnatch 2" in
      assert_equal "pen: [1.1, 2.2, 4.4]" result );
    (* PenSnatch - Remove element from a list containing boolean values *)
    ( "PenSnatch remove boolean element" >:: fun _ ->
      let result = interp "[true; false; true] pensnatch 1" in
      assert_equal "pen: [true, true]" result );
    (* PenSnatch - Remove first element from a list of all identical values *)
    ( "PenSnatch remove from identical values list" >:: fun _ ->
      let result = interp "[42; 42; 42; 42] pensnatch 0" in
      assert_equal "pen: [42, 42, 42]" result );
    (* PenSqueal - Get first element from a Pen list *)
    ( "PenSqueal get first element" >:: fun _ ->
      let result = interp "[1; 2; 3] pensqueal" in
      assert_equal "1" result );
    (* PenSqueal - Get first string element *)
    ( "PenSqueal get first string element" >:: fun _ ->
      let result = interp "[\"apple\"; \"banana\"; \"cherry\"] pensqueal" in
      assert_equal "apple" result );
    (* PenSqueal - Get first element from nested Pen list *)
    ( "PenSqueal get first element from nested Pen list" >:: fun _ ->
      let result = interp "[[1; 2]; [3; 4]] pensqueal" in
      assert_equal "pen: [1, 2]" result );
    (* PenSqueal - Get first float element *)
    ( "PenSqueal get first float element" >:: fun _ ->
      let result = interp "[1.1; 2.2; 3.3] pensqueal" in
      assert_equal "1.1" result );
    (* PenSqueal - Get first boolean element *)
    ( "PenSqueal get first boolean element" >:: fun _ ->
      let result = interp "[true; false; true] pensqueal" in
      assert_equal "true" result );
    (* PenSqueal - PenSqueal from single-element list *)
    ( "PenSqueal from single-element list" >:: fun _ ->
      let result = interp "[42] pensqueal" in
      assert_equal "42" result );
    (* PenSqueal - PenSqueal after evaluating a complex expression *)
    ( "PenSqueal after evaluating a complex expression" >:: fun _ ->
      let result =
        interp "[1 pigpile 1; 3 mudmultiply 4; 5 snoutout 2] pensqueal"
      in
      assert_equal "2" result );
    (* PenLength - Get length of a list with integers *)
    ( "PenLength list of integers" >:: fun _ ->
      let result = interp "[1; 2; 3] penlength" in
      assert_equal "3" result );
    (* PenLength - Get length of a list with strings *)
    ( "PenLength list of strings" >:: fun _ ->
      let result = interp "[\"apple\"; \"banana\"; \"cherry\"] penlength" in
      assert_equal "3" result );
    (* PenLength - Get length of a list with mixed data types *)
    ( "PenLength list with mixed data types" >:: fun _ ->
      let result = interp "[1; \"banana\"; 2.5; true] penlength" in
      assert_equal "4" result );
    (* PenLength - Get length of an empty list *)
    ( "PenLength of an empty list" >:: fun _ ->
      let result = interp "[] penlength" in
      assert_equal "0" result );
    (* PenLength - Get length of a nested list *)
    ( "PenLength of a nested list" >:: fun _ ->
      let result = interp "[[1; 2]; [3; 4]; [5; 6]] penlength" in
      assert_equal "3" result );
    (* PenLength - Get length of a single-element list *)
    ( "PenLength of a single-element list" >:: fun _ ->
      let result = interp "[42] penlength" in
      assert_equal "1" result );
    (* PenLength - Get length after evaluating a list expression *)
    ( "PenLength after evaluating a list expression" >:: fun _ ->
      let result =
        interp "[1 pigpile 1; 3 snoutout 1; 5 mudmultiply 2] penlength"
      in
      assert_equal "3" result );
    (* PenLength - Get length of a list containing another list *)
    ( "PenLength of a list containing another list" >:: fun _ ->
      let result = interp "[1; [2; 3]; 4] penlength" in
      assert_equal "3" result );
    (* PenLength - Get length of a boolean list *)
    ( "PenLength of a boolean list" >:: fun _ ->
      let result = interp "[true; false; true] penlength" in
      assert_equal "3" result ~printer:(fun x -> x) );
    ( "Test if statement with true conditon" >:: fun _ ->
      let result = interp "if true {1} else {2}" in
      assert_equal "1" result ~printer:(fun x -> x) );
    ( "Test if statement with false conditon" >:: fun _ ->
      let _ = interp "oink x = 15;" in
      let result = interp "if x = 5 {2} else {x}" in
      assert_equal "15" result ~printer:(fun x -> x) );
    ( "Test if statement with no else with true condition" >:: fun _ ->
      let _ = interp "oink x = 11;" in
      let result = interp "if x = 11 {x}" in
      assert_equal "11" result ~printer:(fun x -> x) );
    ( "Test if statement with no else with false condition" >:: fun _ ->
      let _ = interp "oink x = 11;" in
      let result = interp "if x =1 {x}" in
      assert_equal "Squeal" result ~printer:(fun x -> x) );
    ( "test workhorse with math operations" >:: fun _ ->
      let result = interp "workhorse add_example x # baaa 5 + x#" in
      let evaluated = interp "go!\n       add_example 3" in
      assert_equal "Squeal" result ~printer:(fun x -> x);
      assert_equal "8" evaluated ~printer:(fun x -> x) );
    ( "test workhorse 2 inputs with math operations addition" >:: fun _ ->
      let result = interp "workhorse add_example [x;y] # baaa x + y#" in
      let evaluated = interp "go! add_example [3;4]" in
      assert_equal "Squeal" result ~printer:(fun x -> x);
      assert_equal "7" evaluated ~printer:(fun x -> x) );
    ( "test workhorse 2 inputs with math operations multiplication" >:: fun _ ->
      let result = interp "workhorse add_example [x;y] # baaa x * y#" in
      let evaluated = interp "go! add_example [3;4]" in
      assert_equal "Squeal" result ~printer:(fun x -> x);
      assert_equal "12" evaluated ~printer:(fun x -> x) );
    ( "test workhorse 2 inputs with math operations division" >:: fun _ ->
      let result = interp "workhorse add_example [x;y] # baaa x/y#" in
      let evaluated = interp "go! add_example [8;4]" in
      assert_equal "Squeal" result ~printer:(fun x -> x);
      assert_equal "2" evaluated ~printer:(fun x -> x) );
    ( "test workhorse 3 inputs with math operations multiplication" >:: fun _ ->
      let result = interp "workhorse add_example [x;y;z] # baaa x*y*z#" in
      let evaluated = interp "go! add_example [2;4;3]" in
      assert_equal "Squeal" result ~printer:(fun x -> x);
      assert_equal "24" evaluated ~printer:(fun x -> x) );
    ( "test workhorse with simple body" >:: fun _ ->
      let result = interp "workhorse test x # oink y = 4; baaa y+x#" in
      let evaluated = interp "go! test 3" in
      assert_equal "Squeal" result ~printer:(fun x -> x);
      assert_equal "7" evaluated ~printer:(fun x -> x) );
    ( "test workhorse with if statements" >:: fun _ ->
      let result =
        interp
          "workhorse test x # \n\
          \  if x = 0 {oink y = 3;}\n\
          \  else {oink y =4;}\n\
           baaa y#"
      in
      let evaluated = interp "go! test 0" in
      assert_equal "Squeal" result ~printer:(fun x -> x);
      assert_equal "3" evaluated ~printer:(fun x -> x);
      let evaluated = interp "go! test 1" in
      assert_equal "4" evaluated ~printer:(fun x -> x) );
    ( "test workhorse with simple (more than one expr) body" >:: fun _ ->
      let result = interp "workhorse test x # x; x baaa x#" in
      let evaluated = interp "go! test 3" in
      assert_equal "Squeal" result ~printer:(fun x -> x);
      assert_equal "3" evaluated ~printer:(fun x -> x) );
    ( "test workhorse with simple (more than one expr) body" >:: fun _ ->
      let result = interp "workhorse test x # oink y = 3;; x baaa x+y#" in
      let evaluated = interp "go! test 3" in
      assert_equal "Squeal" result ~printer:(fun x -> x);
      assert_equal "6" evaluated ~printer:(fun x -> x) );
    ( "test workhorse with oink glob in function) body" >:: fun _ ->
      let result = interp "workhorse test x # oink y = 3; baaa x+y#" in
      let evaluated = interp "go! test 3" in
      assert_equal "Squeal" result ~printer:(fun x -> x);
      assert_equal "6" evaluated ~printer:(fun x -> x) );
    ( "test workhorse with if in body" >:: fun _ ->
      let result =
        interp
          "workhorse test [x;z] # oink y = x + z;; if y = 7 {oink h = 1;} else \
           {oink h = 0;} baaa h#"
      in
      let evaluated = interp "go! test [3;4]" in
      assert_equal "Squeal" result ~printer:(fun x -> x);
      assert_equal "1" evaluated ~printer:(fun x -> x) );
    ( "test setting variable to function output" >:: fun _ ->
      let result = interp "workhorse test x # baaa x#" in
      let _ = interp "oink s = go! test 0;" in
      let evaluated_res = interp "s" in
      assert_equal "Squeal" result ~printer:(fun x -> x);
      assert_equal "0" evaluated_res ~printer:(fun x -> x) );
    ( "test workhorse recursive" >:: fun _ ->
      let result =
        interp
          "workhorse\n\
          \       test [x;z] # oink y = x - 1;; if y = 0 {oink f = z;} else  \
           {oink f = go!\n\
          \       test [y;z+1];} baaa f#"
      in
      let evaluated = interp "go! test [3;0]" in
      assert_equal "Squeal" result ~printer:(fun x -> x);
      assert_equal "2" evaluated ~printer:(fun x -> x) );
  ]

(* TODO TEST division MORE *)
(* TODO TEST EDGE CASES FOR MATH OPERATIONS (int + float etc)*)
let more_func_tests =
  [
    ( "if statements " >:: fun _ ->
      let _ = interp "oink x = 4;" in
      let result = interp "x - 1 = 3" in
      assert_equal "true" result ~printer:(fun x -> x) );
    (* ( "test workhorse recursive with math operations" >:: fun _ -> let result
       = interp "workhorse recursive x # \n\ \ if x - 1 = 0 #oink y = 3#\n\ \
       else ##\n\ \ baaa 5 + x#" in let evaluated = interp "go! add_example 3"
       in assert_equal "Squeal" result ~printer:(fun x -> x); assert_equal "8"
       evaluated ~printer:(fun x -> x) ); *)
  ]

let combined_tests = all_tests @ more_func_tests
let tests = "test suite" >::: combined_tests

let pen_tests =
  [
    ( "test penfilter operation" >:: fun _ ->
      let result =
        interp
          "workhorse filter_fn x # if x = 2 {oink f = true;} else {oink f = \
           false;} baaa f#"
      in
      let _ =
        interp "oink filtered_list = [1; 2; 3; 4] penfilter go! filter_fn;"
      in
      let evaluated_res = interp "filtered_list" in
      assert_equal "Squeal" result ~printer:(fun x -> x);
      assert_equal "pen: [3, 4]" evaluated_res ~printer:(fun x -> x) );
  ]

let _ = run_test_tt_main tests
