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
  | Bool bol -> "what to do"

(** [is_value e] is whether [e] is a value*)
let is_value : expr -> bool = function
  | Int _ | Float _ | String _ | Bool _ -> true

(**[step e] takes a sigle step of evaluation of [e].*)
let rec step : expr -> expr = function
  | Int i -> failwith "Does not step"
  | Float f -> failwith "Does not step"
  | String s -> failwith "Does not step"
  | Bool b -> failwith "Does not step"

(**[eval e] fully evaluates [e] to a value [v].*)
let rec eval (e : expr) : expr = if is_value e then e else e |> step |> eval

(**[interp s] interprets [s] by lexing and parsing it, evaluating it, and
   converting the result to a string.*)
let interp (s : string) : string =
  try s |> parse |> eval |> string_of_val with
  | Failure msg -> "Error: " ^ msg
  | _ -> "An unexpected error occurred while interpreting."
