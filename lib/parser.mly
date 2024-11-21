%token OINK
%token WORKHORSE
%token BAAA
%token GO
%token <string> IDENT
%token <string> MOT
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


%start <Ast.expr> prog
%type <Ast.expr> expr

%%

prog:
| e = expr; EOF { e }
;

expr:
| OINK id = IDENT EQ e1 = expr MUD e2 = expr {Oink (id, e1, e2)}
| OINK id = IDENT EQ e1 = expr {OinkGlob (id, e1)}
| WORKHORSE id = IDENT mot = MOT GATE body=expr BAAA return=expr GATE {OinkGlob (id,Workhorse (mot,body,return))}
| WORKHORSE mot = MOT GATE body=expr BAAA return=expr GATE {Workhorse (mot,body,return)}
| GO f = expr {Go f}
| id=IDENT { Ident id }
| s = STRING {String s}
| i = INT { Int i }
| x = FLOAT {Float x}
| TRUE {Boolean true}
| FALSE {Boolean false}
| e1=expr AND e2=expr { And (e1, e2) }
| e1=expr OR e2=expr { Or (e1, e2) }

;


