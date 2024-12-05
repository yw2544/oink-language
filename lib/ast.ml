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
  (*Pen (List)*)
  | Pen of expr list
  | PenVal of expr list
  | PenPen of expr * expr (* Append to Pen list *)
  | Ppen of expr * expr (* Prepend to Pen list *)
  | PenSnatch of expr * expr (* Remove element from Pen list *)
  | PenSqueal of expr
  | PenLength of expr (* Get length of Pen list *)
  | PenFilter of expr * expr (* Filter elements of Pen list *)
  | PenReap of expr * expr (* Apply function to elements of Pen list *)
  (*Math operation*)
  | PigPile of expr * expr (* Addition *)
  | SnoutOut of expr * expr (* Subtraction *)
  | MudMultiply of expr * expr (* Multiplication *)
  | TroughSplit of expr * expr
  | Eq of expr * expr
(* Equality check *)
(* Division *)

(* type statement = Oink of string * expr * expr *)
