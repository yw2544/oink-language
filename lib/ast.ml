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
  | Workhorse of expr * expr * expr
  | WorkhorseVal of expr * (string, expr) Hashtbl.t
  | Go of string * expr
  | Pen of expr list
  | PenVal of expr list
  | PigPile of expr * expr (* Addition *)
  | SnoutOut of expr * expr (* Subtraction *)
  | MudMultiply of expr * expr (* Multiplication *)
  | TroughSplit of expr * expr
(* Division *)

(* type statement = Oink of string * expr * expr *)
