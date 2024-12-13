val global_env : (string, Ast.expr) Hashtbl.t
val parse : string -> Ast.expr
val string_of_val : Ast.expr -> string
val is_value : Ast.expr -> bool
val print_hashtable : (string, Ast.expr) Hashtbl.t -> unit

val oink_sub :
  string -> Ast.expr -> Ast.expr -> (string, Ast.expr) Hashtbl.t -> Ast.expr

val step : Ast.expr -> (string, Ast.expr) Hashtbl.t -> Ast.expr
val apply : string -> Ast.expr list -> (string, Ast.expr) Hashtbl.t -> Ast.expr
val pen_eval : Ast.expr list -> (string, Ast.expr) Hashtbl.t -> Ast.expr

val pen_eval_lst :
  Ast.expr list -> (string, Ast.expr) Hashtbl.t -> Ast.expr list

val if_eval :
  Ast.expr -> Ast.expr -> Ast.expr -> (string, Ast.expr) Hashtbl.t -> Ast.expr

val eval : Ast.expr -> (string, Ast.expr) Hashtbl.t -> Ast.expr
val interp : string -> string
