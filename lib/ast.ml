type expr =
  | Int of int
  | String of string
  | Float of float
  | Boolean of bool
  | Ident of string
  | Oink of string * expr * expr
  | And of expr * expr
  | Or of expr * expr

(* type statement = Oink of string * expr * expr *)
