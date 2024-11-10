open Final.Main_utils
open Final.Ast
open Printf
module StringMap = Map.Make (String)

let symbol_table = ref StringMap.empty

let print_pig () =
  print_endline "  \\   ^__^";
  print_endline "   \\  (oo)\\\\_______";
  print_endline "      (__)\\\\       )\\/\\";
  print_endline "          ||----w |"

let pigify (s : string) : string =
  let is_vowel c = List.mem c [ 'a'; 'e'; 'i'; 'o'; 'u'; 'y' ] in
  let len = String.length s in
  if len = 0 then s
  else
    let rec find_first_vowel i =
      if i >= len then -1
      else if is_vowel s.[i] then i
      else find_first_vowel (i + 1)
    in
    match find_first_vowel 0 with
    | -1 -> s ^ "yay"
    | 0 -> s ^ "yay"
    | pos ->
        let prefix = String.sub s 0 pos in
        let suffix = String.sub s pos (len - pos) in
        suffix ^ prefix ^ "ay"

let pig_translate (ast : expr) : string =
  match ast with
  | String s ->
      let words = Str.split (Str.regexp "[ ]+") s in
      let pig_words = List.map (fun word -> pigify word) words in
      let pig_words_string = String.concat " " pig_words in
      let result = "*SNORT*" ^ pig_words_string ^ "*SNORT*" in
      result
  | And (e1, e2) -> (
      match (e1, e2) with
      | Boolean b1, Boolean b2 ->
          let result = if b1 && b2 then "true" else "false" in
          "*OINK* And result: " ^ result ^ " *OINK*"
      | _ -> "*OINK* Type error in And expression! *OINK*")
  | Or (e1, e2) -> (
      match (e1, e2) with
      | Boolean b1, Boolean b2 ->
          let result = if b1 || b2 then "true" else "false" in
          "*OINK* Or result: " ^ result ^ " *OINK*"
      | _ -> "*OINK* Type error in Or expression! *OINK*")
  | Ident id -> (
      try
        match StringMap.find id !symbol_table with
        | String s -> "*SNORT* " ^ s ^ " *SNORT*"
        | Int i -> "*SNORT* " ^ string_of_int i ^ " *SNORT*"
        | Float f -> "*SNORT* " ^ string_of_float f ^ " *SNORT*"
        | Boolean b -> "*SNORT* " ^ string_of_bool b ^ " *SNORT*"
        | _ -> "*SNORT* Sorry, I can't understand that yet! Oink Oink~"
      with Not_found -> "*SNORT* Unbound identifier: " ^ id ^ " *SNORT*")
  | Oink (id, e1, e2) ->
      let value = eval e1 in
      symbol_table := StringMap.add id value !symbol_table;
      let value_str =
        match value with
        | String s -> s
        | Int i -> string_of_int i
        | Float f -> string_of_float f
        | Boolean b -> string_of_bool b
        | _ -> "Not supported value type"
      in
      "*OINK* Identifier " ^ id ^ " defined as " ^ value_str ^ " ! *OINK*"
  | _ ->
      "*SNORT* Sorry, I can only interpret strings and boolean now! Oink Oink~"

let main () =
  print_endline "Welcome to the Pig Interpreter! Oink oink~";
  print_pig ();

  let rec repl () =
    print_string
      "You may do one of the following:\n\
       1) Enter a string with quotes (e.g. \"hi\"))for piggy to interpret\n\
       2) Write a extremely simple oink expression (let expression \
       equivalent). The Syntax is oink n = e1 mud e2. \n\
      \   a) After defining an identifier, can print out the identifier value \
       by simplying type in the identifier name\n\
       3) Write a boolean And/Or expression (e.g. true and false).\n\
       4) type 'exit' to quit )): ";
    let input = read_line () in
    if input = "exit" then print_endline "Byebye! Oink oink~"
    else
      try
        (*let interped = interp input in print_endline ("result: " ^
          interped);*)
        let parsed = parse input in
        let piggy_result = pig_translate parsed in
        print_endline piggy_result;
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
