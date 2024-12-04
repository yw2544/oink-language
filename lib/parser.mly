%token OINK
%token WORKHORSE
%token BAAA
%token GO
%token GATE
%token <string> IDENT
%token EQ
%token MUD
%token <int> INT
%token <string> STRING
%token <float> FLOAT 
%token TRUE
%token FALSE
%token EOF
%token AND
%token OR
%token SEP
%token PEN_START
%token PEN_END
%token PIGPILE 
%token SNOUTOUT  
%token MUDMULTIPLY
%token TROUGHSPLIT

%start <Ast.expr> prog
%type <Ast.expr> expr
%type <Ast.expr list> expr_list


%%

expr_list:
  | expr SEP expr_list { $1 :: $3 }  (* Use $3 instead of $2 *)
  | expr { [$1] }  
;
prog:
| e = expr; EOF { e }
;

expr:
| OINK id = IDENT EQ e1 = expr MUD e2 = expr {Oink (id, e1, e2)}
| OINK id = IDENT EQ e1 = expr SEP {OinkGlob (id, e1)}
| PEN_START lst = expr_list PEN_END {Pen lst}
// various workhorse syntactic suggar
| WORKHORSE id = IDENT mot = IDENT GATE body=expr BAAA return=expr GATE {OinkGlob (id,Workhorse (Pen [Ident mot],body,return))}
| WORKHORSE id = IDENT mot = IDENT GATE BAAA return=expr GATE {OinkGlob (id,Workhorse (Pen [Ident mot],Squeal,return))}

| WORKHORSE id = IDENT PEN_START lst = expr_list PEN_END GATE BAAA return=expr GATE {OinkGlob(id, Workhorse (Pen lst,Squeal,return))} 
| WORKHORSE id = IDENT PEN_START lst = expr_list PEN_END GATE body=expr BAAA return=expr GATE {OinkGlob(id,Workhorse (Pen lst,body,return))} 

| WORKHORSE mot = IDENT GATE body=expr BAAA return=expr GATE {Workhorse (Pen [Ident mot],body,return)}
| WORKHORSE mot = IDENT GATE BAAA return=expr GATE {Workhorse (Pen [Ident mot],Squeal,return)}

| WORKHORSE PEN_START lst = expr_list PEN_END GATE BAAA return=expr GATE {Workhorse (Pen lst,Squeal,return)} 
| WORKHORSE PEN_START lst = expr_list PEN_END GATE body=expr BAAA return=expr GATE {Workhorse (Pen lst,body,return)} 
| GO id= IDENT PEN_START lst = expr_list PEN_END {Go (id, Pen lst)}
| GO id=IDENT x=expr {Go (id, x)}
| id=IDENT { Ident id }
| s = STRING {String s}
| i = INT { Int i }
| x = FLOAT {Float x}
| TRUE {Boolean true}
| FALSE {Boolean false}
| e1=expr AND e2=expr { And (e1, e2) }
| e1=expr OR e2=expr { Or (e1, e2) }
| e1 = expr PIGPILE e2 = expr { PigPile (e1, e2) }
| e1 = expr SNOUTOUT e2 = expr { SnoutOut (e1, e2) }
| e1 = expr MUDMULTIPLY e2 = expr { MudMultiply (e1, e2) }
| e1 = expr TROUGHSPLIT e2 = expr { TroughSplit (e1, e2) }
;


