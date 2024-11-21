type expr =
  | Int of int
  | String of string
  | Float of float
  | Boolean of bool
  | Ident of string
  | Mot of string
  | Oink of string * expr * expr
  | OinkGlob of string * expr
  | And of expr * expr
  | Or of expr * expr
  | Workhorse of string * expr * expr
  | Go of expr

(* type statement = Oink of string * expr * expr *)
