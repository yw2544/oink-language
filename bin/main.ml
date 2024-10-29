open Final.Main_utils
open Final.Ast
open Printf

let print_pig () =
  print_endline "  \\   ^__^";
  print_endline "   \\  (oo)\\\\_______";
  print_endline "      (__)\\\\       )\\/\\";
  print_endline "          ||----w |"

(*implement pigify later*)
let pigify s = s

let pig_translate (ast : expr) : string =
  match ast with
  | String s ->
      let words = Str.split (Str.regexp "[ ]+") s in
      let pig_words = List.map (fun word -> pigify word) words in
      let pig_words_string = String.concat " " pig_words in
      let result = "*SNORT*" ^ pig_words_string ^ "*SNORT*" in
      result
  | _ -> "*SNORT* Sorry, I can only interpret strings now! Oink Oink~"

let main () =
  print_endline "Welcome to the Pig Interpreter! Oink oink~";
  print_pig ();

  let rec repl () =
    print_string
      "You may do one of the following:\n\
      \      1) Enter a string with quotes (e.g. \"hi\"))for piggy to \
       interpret\n\n\
      \      2) Write a extremely simple oink expression (let expression \
       equivalent). The Syntax is oink n = e1 mud e2.contents.contents. \n\
      \      3) type 'exit' to quit )): ";
    let input = read_line () in
    if input = "exit" then print_endline "Byebye! Oink oink~"
    else
      try
        let interped = interp input in
        print_endline ("result: " ^ interped);
        let parsed = parse input in
        let piggy_words = pig_translate parsed in
        print_endline piggy_words;
        print_pig ();
        repl ()
      with
      | Failure msg ->
          print_endline ("Error:" ^ msg);
          repl ()
      | Final.Lexer.SyntaxError msg ->
          print_endline ("Syntax Error: " ^ msg);
          repl ()
  in
  repl ()

let () = main ()
