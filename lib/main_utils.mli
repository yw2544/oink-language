(** The main functionality of the Oink interpreter, including parsing,
    evaluation, and variable management. *)

val global_env : (string, Ast.expr) Hashtbl.t
(** [global_env] is the global environment storing variable bindings. It maps
    variable names to their associated expressions. *)

val parse : string -> Ast.expr
(** [parse s] parses the input string [s] into an abstract syntax tree (AST). *)

val string_of_val : Ast.expr -> string
(** [string_of_val v] converts the expression [v] into a human-readable string.
*)

val is_value : Ast.expr -> bool
(** [is_value e] determines if the expression [e] is a value. *)

val print_hashtable : (string, Ast.expr) Hashtbl.t -> unit
(** [print_hashtable tbl] prints the contents of the hash table [tbl] to the
    standard output. *)

val oink_sub :
  string -> Ast.expr -> Ast.expr -> (string, Ast.expr) Hashtbl.t -> Ast.expr
(** [oink_sub id e1 e2 env] evaluates an Oink assignment expression of the form
    [oink id = e1 mud e2] in the given environment [env].

    - [id] is the variable to bind.
    - [e1] is the first expression.
    - [e2] is the second expression.
    - [env] is the current evaluation environment. *)

val step : Ast.expr -> (string, Ast.expr) Hashtbl.t -> Ast.expr
(** [step e env] performs a single-step evaluation of the expression [e] in the
    given environment [env]. *)

val apply : string -> Ast.expr list -> (string, Ast.expr) Hashtbl.t -> Ast.expr
(** [apply id args env] applies the function or workhorse identified by [id] to
    the arguments [args] in the environment [env]. *)

val pen_eval : Ast.expr list -> (string, Ast.expr) Hashtbl.t -> Ast.expr
(** [pen_eval lst env] evaluates the list of expressions [lst] as a Pen (Oink
    list) in the given environment [env]. *)

val pen_eval_lst :
  Ast.expr list -> (string, Ast.expr) Hashtbl.t -> Ast.expr list
(** [pen_eval_lst lst env] evaluates each element of the list [lst] in the given
    environment [env]. *)

val if_eval :
  Ast.expr -> Ast.expr -> Ast.expr -> (string, Ast.expr) Hashtbl.t -> Ast.expr
(** [if_eval cond then_expr else_expr env] evaluates an if-expression in the
    given environment [env].

    - [cond] is the condition.
    - [then_expr] is evaluated if the condition is true.
    - [else_expr] is evaluated if the condition is false. *)

val eval : Ast.expr -> (string, Ast.expr) Hashtbl.t -> Ast.expr
(** [eval e env] fully evaluates the expression [e] in the given environment
    [env] until a value is produced. *)

val interp : string -> string
(** [interp s] interprets the input string [s] by parsing and evaluating it,
    returning the result as a string. *)
