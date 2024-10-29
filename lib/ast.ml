type expr =
  | Int of int
  | String of string
  | Float of float
  | Bool of bool
  | Ident of string
  | Oink of string * expr * expr

(* type statement = Oink of string * expr * expr *)
