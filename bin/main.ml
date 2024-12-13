open Final.Main_utils
open Final.Ast
open Printf
module StringMap = Map.Make (String)

let print_pig () =
  print_endline "  \\    ^ _ _ ^";
  print_endline "   \\  (  oo    )\\\\_______";
  print_endline "        (______)\\\\       )\\/\\";
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
        match Hashtbl.find global_env id with
        | String s -> "*SNORT* " ^ s ^ " *SNORT*"
        | Int i -> "*SNORT* " ^ string_of_int i ^ " *SNORT*"
        | Float f -> "*SNORT* " ^ string_of_float f ^ " *SNORT*"
        | Boolean b -> "*SNORT* " ^ string_of_bool b ^ " *SNORT*"
        | _ -> "*SNORT* Sorry, I can't understand that yet! Oink Oink~"
      with Not_found -> "*SNORT* Unbound identifier: " ^ id ^ " *SNORT*")
  | Oink (id, e1, _) | OinkGlob (id, e1) ->
      let value = eval e1 global_env in
      Hashtbl.add global_env id value;
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
      "*SNORT* Sorry, I can only interpret strings, variables, boolean, and \
       file now! Oink Oink~"

let file_parser (filename : string) : unit =
  try
    let in_channel = open_in filename in
    let rec process_lines buffer =
      match input_line in_channel with
      | exception End_of_file ->
          if buffer <> "" then process_line (String.trim buffer);
          close_in in_channel;
          print_endline "*OINK* Finished processing file! *OINK*"
      | line ->
          let trimmed_line = String.trim line in
          if trimmed_line = "" || String.starts_with ~prefix:"#" trimmed_line
          then process_lines buffer
          else if String.ends_with ~suffix:";" trimmed_line then (
            let full_line =
              buffer ^ " "
              ^ String.sub trimmed_line 0 (String.length trimmed_line - 1)
            in
            process_line (String.trim full_line);
            process_lines "")
          else process_lines (buffer ^ " " ^ trimmed_line)
    and process_line line =
      try
        print_endline ("Processing: " ^ line);
        let result = interp line in
        print_endline ("Result: " ^ result)
      with
      | Failure msg -> print_endline ("Error: " ^ msg)
      | Final.Lexer.SyntaxError msg -> print_endline ("Syntax Error: " ^ msg)
    in
    process_lines ""
  with Sys_error msg -> print_endline ("Error opening file: " ^ msg)

let main () =
  print_endline "Welcome to the Pig Interpreter! Oink oink~";
  print_pig ();

  let rec repl () =
    print_string
      "You may do one of the following:\n\
       1) Enter a string with quotes (e.g. \"hi\") for piggy to interpret\n\
       2) Write an Oink expression (let expression equivalent) to define a \
       variable. The Syntax is\" oink n = e1 mud e2\" or \"oink y = 5;\". \n\
       NOTE: \" ; \" is REQUIRED for \"oink x = XX; \" not required if using \
       \"oink x = XX mud XX\" \n\
      \ that is, if you have \"mud\" no need to add \"; \" at the end\n\
       3) View the list of defined vairbale by typing 'env'.\n\
       4) Remove a variable by typing 'remove <var>'.\n\
       5) Write a boolean And/Or expression (e.g. true and false).\n\
       6) View the supported operations for our pen (list) by typing 'list \
       help' in oink file ONLY. Not supported in driver program.\n\
       7) Parse a oink file by typing 'file folder/<filename>'.\n\
       8) type 'exit' to quit )): ";
    let input = read_line () in
    if input = "exit" then print_endline "Byebye! Oink oink~"
    else if input = "list help" then (
      print_endline "*OINK* List Operations Guide *OINK*";
      print_endline "Oink supports the following list (Pen) operations:";
      print_endline "1. PenPen: Concatenates two lists.";
      print_endline "   Syntax: [list1] penpen [list2]";
      print_endline "   Example: [1; 2] penpen [3; 4]";
      print_endline "   Result: pen: [1, 2, 3, 4]";
      print_endline "";
      print_endline "2. Ppen: Prepends an element to a list.";
      print_endline "   Syntax: element ppen [list]";
      print_endline "   Example: 1 ppen [2; 3]";
      print_endline "   Result: pen: [1, 2, 3]";
      print_endline "";
      print_endline "3. PenSnatch: Removes an element by index.";
      print_endline "   Syntax: [list] pensnatch index";
      print_endline "   Example: [1; 2; 3] pensnatch 1";
      print_endline "   Result: pen: [1, 3]";
      print_endline "";
      print_endline "4. PenSqueal: Gets the first element.";
      print_endline "   Syntax: [list] pensqueal";
      print_endline "   Example: [1; 2; 3] pensqueal";
      print_endline "   Result: 1";
      print_endline "";
      print_endline "5. PenLength: Gets the length of the list.";
      print_endline "   Syntax: [list] penlength";
      print_endline "   Example: [1; 2; 3] penlength";
      print_endline "   Result: 3";
      print_endline "";
      print_endline
        "*OINK* Try these operations in the REPL or your Oink file! *OINK*";
      print_pig ();
      repl ())
    else if input = "env" then (
      print_endline "Current list of defined variables:";
      print_hashtable global_env;
      print_pig ();
      repl ())
    else if String.length input >= 7 && String.sub input 0 7 = "remove " then (
      let var = String.sub input 7 (String.length input - 7) in
      if Hashtbl.mem global_env var then (
        Hashtbl.remove global_env var;
        print_endline ("*OINK* Removed " ^ var ^ " from the environment! *OINK*");
        print_pig ())
      else (
        print_endline
          ("*OINK* Cannot remove because no such variable: " ^ var ^ " *SNORT*");
        print_pig ());
      repl ())
    else if String.length input >= 5 && String.sub input 0 5 = "file " then (
      let filename = String.sub input 5 (String.length input - 5) in
      file_parser filename;
      repl ())
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
