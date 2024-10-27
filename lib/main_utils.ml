open Ast

(**[parse s] parses [s] into an AST.*)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(**[interp s] interprets [s] by lexing and parsing it, evaluating it, and
   converting the result to a string.*)
let interp (s : string) : string = failwith "unimpelmented"
