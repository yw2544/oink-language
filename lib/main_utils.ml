open Ast

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
  | Bool b -> string_of_bool b
  | Oink (id, e1, e2) -> "oink"
  | Ident _ -> "ident"

(** [is_value e] is whether [e] is a value*)
let is_value : expr -> bool = function
  | Int _ | Float _ | String _ | Bool _ -> true
  | Oink _ -> false
  | Ident _ -> false

type valTypes =
  | Int
  | Float
  | String
  | Bool

let rec oink_sub id value expr =
  match expr with
  | Ident x when x = id -> value
  | Ident x -> Ident x
  | Oink (x, e1, e2) when x = id -> Oink (x, oink_sub id value e1, e2)
  | Oink (x, e1, e2) when x <> id ->
      Oink (x, oink_sub id value e1, oink_sub id value e2)
  | _ -> failwith "does not step"

(**[step e] takes a sigle step of evaluation of [e].*)
let rec step : expr -> expr = function
  | Int _ -> failwith "does not step"
  | Float _ -> failwith "does not step"
  | String _ -> failwith "does not step"
  | Bool _ -> failwith "does not step"
  | Oink (id, e1, e2) ->
      if is_value e2 then e2
      else if is_value e1 then oink_sub id e1 e2
      else
        let value = step e1 in
        oink_sub id value e2
  | _ -> failwith "unsupported step"

(**[eval e] fully evaluates [e] to a value [v].*)
let rec eval (e : expr) : expr = if is_value e then e else e |> step |> eval

(**[interp s] interprets [s] by lexing and parsing it, evaluating it, and
   converting the result to a string.*)
let interp (s : string) : string =
  try s |> parse |> eval |> string_of_val with
  | Failure msg -> "Error: " ^ msg
  | _ -> "An unexpected error occurred while interpreting."
