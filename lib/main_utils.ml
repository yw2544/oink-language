open Ast

(* Global environment: maps variable names to values *)
let global_env = Hashtbl.create 100

(**[parse s] parses [s] into an AST.*)
let parse (s : string) : expr =
  (* print_endline "start parse"; *)
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  (* print_endline "finished parse"; *)
  ast

(**[string_of_val e] converts [e] to a string Requires: [e] is a value.*)
let string_of_val (e : expr) : string =
  match e with
  | Int i -> string_of_int i
  | String s -> s
  | Float x -> string_of_float x
  | Boolean b -> string_of_bool b
  | Oink (id, e1, e2) -> "oink"
  | Ident _ -> "ident"
  | And (e1, e2) -> "and"
  | Or (e1, e2) -> "or"
  | Squeal -> "Squeal"
  | Workhorse (Pen x, body, ret) -> "workhorse input: currently can't print"
  | Pen lst -> "pen: unsuported rn"
  | _ -> "currently unsupported"

(** [is_value e] is whether [e] is a value*)
let is_value : expr -> bool = function
  | Squeal | Int _ | Float _ | String _ | Boolean _ | Workhorse _ | PenVal _ ->
      true
  | Oink _ | OinkGlob _ | Go _ | And _ | Or _ | Ident _ | Pen _ -> false

let rec oink_sub id value expr outer_env =
  let oink_env = Hashtbl.copy outer_env in
  Hashtbl.add oink_env id value;
  match expr with
  | Ident x when x = id -> value
  | Ident x -> Ident x
  | Oink (x, e1, e2) when x = id -> Oink (x, oink_sub id value e1 outer_env, e2)
  | Oink (x, e1, e2) when x <> id ->
      Oink (x, oink_sub id value e1 outer_env, oink_sub id value e2 outer_env)
  | expr -> step expr outer_env
(* | And (e1, e2) -> And (oink_sub id value e1 outer_env, oink_sub id value e2
   outer_env) | Or (e1, e2) -> Or (oink_sub id value e1 outer_env, oink_sub id
   value e2 outer_env) *)

(**[step e] takes a sigle step of evaluation of [e].*)
and step (e : expr) (env : (string, expr) Hashtbl.t) : expr =
  match e with
  | Int _ -> failwith "does not step"
  | Float _ -> failwith "does not step"
  | String _ -> failwith "does not step"
  | Boolean _ -> failwith "does not step"
  | PenVal _ -> failwith "does not step"
  | Ident x -> Hashtbl.find env x
  | And (e1, e2) when is_value e1 && is_value e2 -> (
      match (e1, e2) with
      | Boolean b1, Boolean b2 ->
          let result = Ast.Boolean (b1 && b2) in
          if b1 && b2 then print_endline "*OINK* Both true, so true! *OINK*"
          else print_endline "*SNORT* One or both false, so false! *SNORT*";
          result
      | _ -> failwith "*OINK* Type error in And expression! Oink Oink~ *OINK*")
  | And (e1, e2) when is_value e1 -> And (e1, step e2 env)
  | And (e1, e2) -> And (step e1 env, e2)
  | Or (e1, e2) when is_value e1 && is_value e2 -> (
      match (e1, e2) with
      | Boolean b1, Boolean b2 ->
          let result = Ast.Boolean (b1 || b2) in
          if b1 || b2 then
            print_endline "*OINK* At least one is true, so true! *OINK*"
          else print_endline "*SNORT* Both false, so false! *SNORT*";
          result
      | _ -> failwith "*OINK* Type error in Or expression! Oink Oink~ *OINK*")
  | Or (e1, e2) when is_value e1 -> Or (e1, step e2 env)
  | Or (e1, e2) -> Or (step e1 env, e2)
  | Pen lst -> pen_eval lst env
  | Oink (id, e1, e2) ->
      if is_value e2 then e2
      else if is_value e1 then oink_sub id e1 e2 env
      else
        let value = step e1 env in
        oink_sub id value e2 env
  | OinkGlob (id, e1) ->
      print_endline ("adding to global env: " ^ id);
      Hashtbl.add env id e1;
      Squeal
  | Go (id, PenVal lst) ->
      print_endline "about to call apply";
      apply id lst env
  | Go (id, Pen lst) ->
      print_endline "calling pen eval_lst";
      let lst = pen_eval_lst lst env in
      Printf.printf "IN STEP!! The length of apply_lst is %d\n"
        (List.length lst);
      apply id lst env
  | Go (id, x) -> apply id [ x ] env
  | _ -> failwith "unsupported step"

and apply id apply_lst outer_env =
  print_endline " within apply";
  try
    let func = Hashtbl.find outer_env id in
    let func_env = Hashtbl.copy outer_env in
    match func with
    | Workhorse (Pen def_lst, body, return_expr) ->
        print_endline "matched with workhorse";

        let def_lst_string =
          List.map
            (function
              | Ident id -> id
              | _ -> failwith "Expected Ident in Pen")
            def_lst
        in
        print_endline "before adding to env";
        let add_to_table var_name var_value =
          Hashtbl.add func_env var_name var_value
        in
        List.iter print_endline def_lst_string;
        let print_element e = print_endline (string_of_val e) in
        List.iter print_element apply_lst;
        Printf.printf "The length of apply_lst is %d\n" (List.length apply_lst);
        List.iter2 add_to_table def_lst_string apply_lst;
        print_endline "after adding to env";
        if is_value body = false then
          let _ = step body func_env in
          if is_value return_expr then return_expr
          else step return_expr func_env
        else if is_value return_expr then return_expr
        else step return_expr func_env
    | _ -> failwith "apply: Not a Workhorse function"
  with Not_found ->
    failwith ("apply: Function ID not found: " ^ id ^ " or some other error")

and pen_eval lst env =
  print_endline "within pen_eval";
  print_endline "calling pen eval_lst";

  let lst = pen_eval_lst lst env in
  let print_element e = print_endline (string_of_val e) in
  List.iter print_element lst;
  print_endline "fin eval_lst";

  PenVal lst

and pen_eval_lst lst env =
  let f e = if is_value e then e else step e env in
  Printf.printf "WITHIN PEN_EVAL_LIST The length of input list is %d\n"
    (List.length lst);
  List.map f lst

(**[eval e] fully evaluates [e] to a value [v].*)
let rec eval (e : expr) (env : (string, expr) Hashtbl.t) : expr =
  if is_value e then e else eval (step e env) env

(**[interp s] interprets [s] by lexing and parsing it, evaluating it, and
   converting the result to a string.*)
let interp (s : string) : string =
  (* print_endline "in interp"; *)
  try s |> parse |> (fun e -> eval e global_env) |> string_of_val with
  | Failure msg -> "Error: " ^ msg
  | _ -> "An unexpected error occurred while interpreting."
