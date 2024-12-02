type expr =
  | Squeal
  | Int of int
  | String of string
  | Float of float
  | Boolean of bool
  | Ident of string
  | Oink of string * expr * expr
  | OinkGlob of string * expr
  | And of expr * expr
  | Or of expr * expr
  | Workhorse of string * expr * expr
  | Go of string * expr
  | Pen of expr list
(* type statement = Oink of string * expr * expr *)
