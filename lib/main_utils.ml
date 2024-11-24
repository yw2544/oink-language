open Ast

(* Global environment: maps variable names to values *)
let global_env = Hashtbl.create 100

(**[parse s] parses [s] into an AST.*)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in

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
  | Workhorse (x,body,ret) -> "workhorse input:" ^ x 
  | _ -> "currently unsupported"

(** [is_value e] is whether [e] is a value*)
let is_value : expr -> bool = function
  |Squeal | Int _ | Float _ | String _ | Boolean _ | Workhorse _-> true
  | Oink _ |OinkGlob _| Go _ | And _ | Or _ -> false
  | Ident _-> false

type valTypes =
  | Int
  | Float
  | String
  | Boolean


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
  (* | And (e1, e2) -> And (oink_sub id value e1 outer_env, oink_sub id value e2 outer_env)
  | Or (e1, e2) -> Or (oink_sub id value e1 outer_env, oink_sub id value e2 outer_env) *)

(**[step e] takes a sigle step of evaluation of [e].*)
and step (e : expr) (env : (string, expr) Hashtbl.t) : expr =
  match e with
  | Int _ -> failwith "does not step"
  | Float _ -> failwith "does not step"
  | String _ -> failwith "does not step"
  | Boolean _ -> failwith "does not step"
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
  | Oink (id, e1, e2) ->
      if is_value e2 then e2
      else if is_value e1 then oink_sub id e1 e2 env
      else
        let value = step e1 env in
        oink_sub id value e2 env
  | OinkGlob (id, e1) -> Hashtbl.add env id e1;Squeal
  (* | Workhorse (mot,body,return) -> Squeal *)
  | Go (id,e1) -> apply id e1 env
  
  | _ -> failwith "unsupported step"
and
apply id value outer_env =
  try
    let func = Hashtbl.find outer_env id in
    let func_env = Hashtbl.copy outer_env in
    match func with
    | Workhorse (mot, body, return_expr) ->
        Hashtbl.add func_env mot value;
        if is_value body = false then (
        let _ = step body func_env in
        if is_value return_expr then return_expr
        else step return_expr func_env  
          )
      else (
        if is_value return_expr then return_expr
      else 
        
        step return_expr func_env)
        
        
    | _ -> failwith "apply: Not a Workhorse function"
  with Not_found ->
    failwith ("apply: Function ID not found: " ^ id^ " or some other error")
(**[eval e] fully evaluates [e] to a value [v].*)
let rec eval (e : expr) (env:(string, expr) Hashtbl.t) : expr = 
  if is_value e then e 
  else eval (step e env) env

(**[interp s] interprets [s] by lexing and parsing it, evaluating it, and
   converting the result to a string.*)
let interp (s : string) : string =
  try s |> parse |> (fun e -> eval e global_env) |> string_of_val with
  | Failure msg -> "Error: " ^ msg
  | _ -> "An unexpected error occurred while interpreting."

