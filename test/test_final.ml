open OUnit2
open Final.Ast
open Final.Main_utils

(**[make_i n i s] makes an OUnit test named [n] that expects [s] to evaluate to
   [i].*)
let make_i n i s = n >:: fun _ -> assert_equal (string_of_int i) (interp s)

let make_s n s1 s2 = n >:: fun _ -> assert_equal s1 (interp s2)
let print_interp input = Printf.printf "Result: %s\n" (interp input)

let make_test_oink n var_name expr1 expr2 exp =
  n >:: fun _ -> assert_equal exp (interp "Oink x = bla mud x") ~printer:Fun.id

let tests =
  "test suite"
  >::: [
         make_i "process integer" 22 "22";
         make_s "process string" "hello world" "\"hello world\"";
         (* make_test_oink "test oink" "x" "bla" "x" "3"; *)
         (* ( "test eval" >:: fun _ -> let result = interp "oink x = 3 mud x" in
            assert_equal "3" result ); *)
         ( "test parse" >:: fun _ ->
           assert_equal (Oink ("x", Int 3, Int 3)) (parse "oink x = 3 mud 3") );
         ( "test parse" >:: fun _ ->
           assert_equal
             (Oink ("x", Int 3, Ident "x"))
             (parse "oink x = 3 mud x") );
       ]

let _ = run_test_tt_main tests
