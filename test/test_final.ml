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
         make_s "process string" "hello world" "print 'hello word'";
       ]

let _ = run_test_tt_main tests
