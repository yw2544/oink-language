open OUnit2
open Final.Ast
open Final.Main_utils

(**[make_i n i s] makes an OUnit test named [n] that expects [s] to evaluate to
   [i].*)
let make_i n i s = n >:: fun _ -> assert_equal (string_of_int i) (interp s)

let tests =
  "test suite" >::: [ ("a trivial test" >:: fun _ -> assert_equal 0 0) ]

let _ = run_test_tt_main tests
