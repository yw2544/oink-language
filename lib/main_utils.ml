open Ast

(* Global environment: maps variable names to values *)
let global_env = Hashtbl.create 100

(**[parse s] parses [s] into an AST.*)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  try let ast = Parser.prog Lexer.read lexbuf in

      ast with
  | Parser.Error ->
      print_endline "Parsing error: Invalid syntax.";
      raise (Failure "Parsing error")
  | _ ->
      print_endline "An unexpected parsing error occurred.";
      raise (Failure "Unexpected parsing error")

(**[string_of_val e] converts [e] to a string Requires: [e] is a value.*)
let rec string_of_val (e : expr) : string =
  match e with
  | Int i -> string_of_int i
  | String s -> s
  | Float x -> Printf.sprintf "%.1f" x
  | Boolean b -> string_of_bool b
  | Pen lst ->
      let s =
        "pen: [" ^ String.concat ", " (List.map string_of_val lst) ^ "]"
      in
      s
  | PenVal lst ->
      let s =
        "pen: [" ^ String.concat ", " (List.map string_of_val lst) ^ "]"
      in
      s
  | Oink (id, e1, e2) -> "oink"
  | Ident _ -> "ident"
  | And (e1, e2) -> "and"
  | Or (e1, e2) -> "or"
  | Squeal -> "Squeal"
  | Workhorse (Pen x, body, ret) -> "workhorse input: currently can't print"
  | PigPile (e1, e2) ->
      "pigpile(" ^ string_of_val e1 ^ ", " ^ string_of_val e2 ^ ")"
  | SnoutOut (e1, e2) ->
      "snoutout(" ^ string_of_val e1 ^ ", " ^ string_of_val e2 ^ ")"
  | MudMultiply (e1, e2) ->
      "mudmultiply(" ^ string_of_val e1 ^ ", " ^ string_of_val e2 ^ ")"
  | TroughSplit (e1, e2) ->
      "troughsplit(" ^ string_of_val e1 ^ ", " ^ string_of_val e2 ^ ")"
  | PenPen (e1, e2) ->
      "penpen(" ^ string_of_val e1 ^ ", " ^ string_of_val e2 ^ ")"
  | Ppen (e1, e2) -> "ppen(" ^ string_of_val e1 ^ ", " ^ string_of_val e2 ^ ")"
  | PenSnatch (e1, e2) ->
      "PenSnatch(" ^ string_of_val e1 ^ ", " ^ string_of_val e2 ^ ")"
  | PenSqueal e -> "PenSqueal(" ^ string_of_val e ^ ")"
  | PenLength e -> "PenLength(" ^ string_of_val e ^ ")"
  | _ -> "currently unsupported"

let is_value : expr -> bool = function
  | Squeal | Int _ | Float _ | String _ | Boolean _ | WorkhorseVal _ | PenVal _
    -> true
  | Oink _ | OinkGlob _ | Go _ | And _ | Or _ | Ident _ | Workhorse _ | Pen _ ->
      false
  | If _ | _ -> false

let print_hashtable hashtable =
  Hashtbl.iter
    (fun key value -> Printf.printf "%s -> %s\n" key (string_of_val value))
    hashtable

let rec oink_sub id value expr outer_env =
  let oink_env = Hashtbl.copy outer_env in
  Hashtbl.add oink_env id value;
  match expr with
  | Ident x when x = id -> value
  | Ident x -> Ident x
  | Oink (x, e1, e2) when x = id -> Oink (x, oink_sub id value e1 outer_env, e2)
  | Oink (x, e1, e2) when x <> id ->
      Oink (x, oink_sub id value e1 outer_env, oink_sub id value e2 outer_env)
  | expr -> eval expr oink_env

(**[step e] takes a sigle step of evaluation of [e].*)
and step (e : expr) (env : (string, expr) Hashtbl.t) : expr =
  match e with
  | Int _ -> failwith "does not step int"
  | Float _ -> failwith "does not step"
  | String _ -> failwith "does not step"
  | Boolean _ -> failwith "does not step"
  | PenVal _ -> failwith "does not step"
  | Ident x -> Hashtbl.find env x
  | Paren e1 -> eval e1 env
  | PigPile (e1, e2) when is_value e1 && is_value e2 -> (
      match (e1, e2) with
      | Int i1, Int i2 -> Int (i1 + i2)
      | Float f1, Float f2 -> Float (f1 +. f2)
      | Int i1, Float f2 -> Float (float_of_int i1 +. f2)
      | Float f1, Int i2 -> Float (f1 +. float_of_int i2)
      | String s1, String s2 -> String (s1 ^ s2)
      | _ -> failwith "Type error in PigPile operation")
  | SnoutOut (e1, e2) when is_value e1 && is_value e2 -> (
      match (e1, e2) with
      | Int i1, Int i2 -> Int (i1 - i2)
      | Float f1, Float f2 -> Float (f1 -. f2)
      | Int i1, Float f2 -> Float (float_of_int i1 -. f2)
      | Float f1, Int i2 -> Float (f1 -. float_of_int i2)
      | _ -> failwith "Type error in SnoutOut operation")
  | MudMultiply (e1, e2) when is_value e1 && is_value e2 -> (
      match (e1, e2) with
      | Int i1, Int i2 -> Int (i1 * i2)
      | Float f1, Float f2 -> Float (f1 *. f2)
      | Int i1, Float f2 -> Float (float_of_int i1 *. f2)
      | Float f1, Int i2 -> Float (f1 *. float_of_int i2)
      | _ -> failwith "Type error in MudMultiply operation")
  | TroughSplit (e1, e2) when is_value e1 && is_value e2 -> (
      match (e1, e2) with
      | Int i1, Int i2 when i2 = 0 -> raise Division_by_zero
      | Float f1, Float f2 when f2 = 0.0 -> raise Division_by_zero
      | Int i1, Int i2 -> Int (i1 / i2)
      | Float f1, Float f2 -> Float (f1 /. f2)
      | Int i1, Float f2 -> Float (float_of_int i1 /. f2)
      | Float f1, Int i2 -> Float (f1 /. float_of_int i2)
      | _ -> failwith "Invalid TroughSplit expression")
  | Eq (e1, e2) -> (
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      match (v1, v2) with
      | Int i1, Int i2 -> Boolean (i1 = i2)
      | Float f1, Float f2 -> Boolean (f1 = f2)
      | String s1, String s2 -> Boolean (s1 = s2)
      | Boolean b1, Boolean b2 -> Boolean (b1 = b2)
      | _, _ -> failwith "*OINK* Type error in Eq expression! Oink Oink~ *OINK*"
      )
  | PenPen (PenVal lst1, PenVal lst2) -> PenVal (lst1 @ lst2)
  | PenPen (Pen lst1, Pen lst2)
    when List.for_all is_value lst1 && List.for_all is_value lst2 ->
      PenVal (lst1 @ lst2)
  | PenPen (Pen lst1, Pen lst2) -> PenPen (pen_eval lst1 env, pen_eval lst2 env)
  | Ppen (x, PenVal lst) -> PenVal (x :: lst)
  | Ppen (x, Pen lst) -> PenVal (x :: lst)
  | PenSnatch (Pen lst, Int i) ->
      if i >= 0 && i < List.length lst then
        Pen (List.filteri (fun idx _ -> idx <> i) lst)
      else failwith "PenSnatch: Index out of bounds"
  | PenSnatch (e1, e2) when is_value e1 -> PenSnatch (e1, step e2 env)
  | PenSnatch (e1, e2) -> PenSnatch (step e1 env, e2)
  | PenSqueal (Pen lst) -> (
      match lst with
      | e :: _ -> e
      | [] -> failwith "PenSqueal: Pen is empty")
  | PenSqueal e -> PenSqueal (step e env)
  | PenLength (Pen lst) -> Int (List.length lst)
  | PenLength e -> PenLength (step e env)
  | PigPile (e1, e2) when is_value e1 -> PigPile (e1, step e2 env)
  | PigPile (e1, e2) when is_value e2 -> PigPile (step e1 env, e2)
  | PigPile (e1, e2) -> PigPile (step e1 env, step e2 env)
  | SnoutOut (e1, e2) when is_value e1 -> SnoutOut (e1, step e2 env)
  | SnoutOut (e1, e2) when is_value e2 -> SnoutOut (step e1 env, e2)
  | SnoutOut (e1, e2) -> SnoutOut (step e1 env, step e2 env)
  | MudMultiply (e1, e2) when is_value e1 -> MudMultiply (e1, step e2 env)
  | MudMultiply (e1, e2) when is_value e2 -> MudMultiply (step e1 env, e2)
  | MudMultiply (e1, e2) -> MudMultiply (step e1 env, step e2 env)
  | TroughSplit (e1, e2) when is_value e1 -> TroughSplit (e1, step e2 env)
  | TroughSplit (e1, e2) when is_value e2 -> TroughSplit (step e1 env, e2)
  | TroughSplit (e1, e2) -> TroughSplit (step e1 env, step e2 env)
  | And (e1, e2) when is_value e1 && is_value e2 -> (
      match (e1, e2) with
      | Boolean b1, Boolean b2 ->
          let result = Ast.Boolean (b1 && b2) in
          result
      | _ -> failwith "*OINK* Type error in And expression! Oink Oink~ *OINK*")
  | And (e1, e2) when is_value e1 -> And (e1, step e2 env)
  | And (e1, e2) -> And (step e1 env, e2)
  | Or (e1, e2) when is_value e1 && is_value e2 -> (
      match (e1, e2) with
      | Boolean b1, Boolean b2 ->
          let result = Ast.Boolean (b1 || b2) in
          result
      | _ -> failwith "*OINK* Type error in Or expression! Oink Oink~ *OINK*")
  | Or (e1, e2) when is_value e1 -> Or (e1, step e2 env)
  | Or (e1, e2) -> Or (step e1 env, e2)
  | Pen lst when List.for_all is_value lst -> PenVal lst
  | Pen lst -> pen_eval lst env
  | Workhorse (inputs, body, outputs) ->
      WorkhorseVal (Workhorse (inputs, body, outputs), Hashtbl.copy env)
  | Oink (id, e1, e2) ->
      if is_value e2 then e2
      else if is_value e1 then oink_sub id e1 e2 env
      else
        let value = step e1 env in
        oink_sub id value e2 env
  | OinkGlob (id, e1) ->
      Hashtbl.add env id (eval e1 env);
      Squeal
  | Go (id, PenVal lst) -> apply id lst env
  | Go (id, Pen lst) ->
      let lst = pen_eval_lst lst env in
      apply id lst env
  | Go (id, x) -> apply id [ x ] env
  | If (e, e1, e2) -> if_eval e e1 e2 env
  | _ -> failwith "unsupported step"

and apply id apply_lst current_outer =
  let find_func_and_env () =
    let func_and_env = Hashtbl.find current_outer id in
    match func_and_env with
    | WorkhorseVal (func, func_env) ->
        Hashtbl.add func_env id func_and_env;
        (func, func_env)
    | _ -> failwith "failure in function application"
  in
  try
    let func_and_env = find_func_and_env () in
    let func = fst func_and_env in
    let func_env = snd func_and_env in
    match func with
    | Workhorse (Pen def_lst, body, return_expr) ->
        let def_lst_string =
          List.map
            (function
              | Ident id -> id
              | _ -> failwith "Expected Ident in Pen")
            def_lst
        in
        let add_to_table var_name var_value =
          Hashtbl.add func_env var_name var_value
        in
        List.iter2 add_to_table def_lst_string apply_lst;
        if not (is_value body) then
          let _ = step body func_env in
          if is_value return_expr then return_expr
          else step return_expr func_env
        else if is_value return_expr then return_expr
        else step return_expr func_env
    | _ -> failwith "apply: Not a Workhorse function"
  with Not_found ->
    failwith ("apply: Function ID not found: " ^ id ^ " or some other error")

and pen_eval lst env =
  if List.for_all is_value lst then PenVal lst
  else
    let evaluated_lst = pen_eval_lst lst env in
    PenVal evaluated_lst

and pen_eval_lst lst env =
  let f e = if is_value e then e else step e env in
  List.map f lst

and if_eval e e1 e2 env =
  let cond = eval e env in
  match cond with
  | Boolean true -> eval e1 env
  | Boolean false -> eval e2 env
  | _ -> failwith "if statement condition evaluation error."

and eval (e : expr) (env : (string, expr) Hashtbl.t) : expr =
  if is_value e then e else eval (step e env) env

(**[interp s] interprets [s] by lexing and parsing it, evaluating it, and
   converting the result to a string.*)
let interp (s : string) : string =
  try s |> parse |> (fun e -> eval e global_env) |> string_of_val with
  | Failure msg -> "Error: " ^ msg
  | Not_found -> "Error: Unbound identifier found during evaluation."
  | Division_by_zero -> raise Division_by_zero
  | Invalid_argument msg -> "Error: Invalid argument: " ^ msg
  | e -> "An unexpected error occurred: " ^ Printexc.to_string e
