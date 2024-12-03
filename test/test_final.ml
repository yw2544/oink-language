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
               "workhorse example x # workhorse exp y #4 baaa y# baaa go! exp \
                4#"
           in
           let evaluated = interp "go! example 3" in
           assert_equal "Squeal" result ~printer:(fun x -> x);
           assert_equal "4" evaluated ~printer:(fun x -> x) );
         ( "test workhorse alternate definition. oink + anonymous function"
         >:: fun _ ->
           let result =
             interp "oink test = workhorse x # oink y = 14; baaa y#;"
           in
           let evaluated = interp "go! test 3" in
           assert_equal "Squeal" result ~printer:(fun x -> x);
           assert_equal "14" evaluated ~printer:(fun x -> x) );
         (* ( "test workhorse anonymous" >:: fun _ -> let result = interp
            "workhorse x #x baaa x#" in assert_equal "workhorse input:x" result
            ~printer:(fun x -> x) ); *)
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
       ]

let _ = run_test_tt_main tests
