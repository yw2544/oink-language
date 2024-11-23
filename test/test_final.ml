open OUnit2
open Final.Ast
open Final.Main_utils

(**[make_i n i s] makes an OUnit test named [n] that expects [s] to evaluate to
   [i].*)
let make_i n i s = n >:: fun _ -> assert_equal (string_of_int i) (interp s)

let make_s n s1 s2 = n >:: fun _ -> assert_equal s1 (interp s2)
let print_interp input = Printf.printf "Result: %s\n" (interp input)

let tests =
  "test suite"
  >::: [
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
           assert_equal
             (Oink ("x", Int 3, Ident "x"))
             (parse "oink x = 3 mud x") );
         ( "test oink no mud" >:: fun _ ->
           let result = interp "oink x = 3;" in
           assert_equal "Squeal" result ~printer:(fun x -> x) );
         ( "test oink no mud evaluated" >:: fun _ ->
           let result = interp "oink x = 11;" in
           let result2 = interp "x" in
           assert_equal "11" result2 ~printer:(fun x -> x);
           assert_equal "Squeal" result ~printer:(fun x -> x) );
         ( "test workhorse" >:: fun _ ->
           let result = interp "workhorse example x #x baaa x#" in
           assert_equal "Squeal" result ~printer:(fun x -> x) );
         ( "test workhorse" >:: fun _ ->
           let result = interp "workhorse x #x baaa x#" in
           assert_equal "workhorse input:x" result ~printer:(fun x -> x) );
       ]

let _ = run_test_tt_main tests
