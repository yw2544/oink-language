%token OINK
%token <string> IDENT
%token EQ
%token MUD
%token <int> INT
%token <string> STRING
%token <float> FLOAT 
%token TRUE
%token FALSE
%token EOF



%start <Ast.expr> prog
%type <Ast.expr> expr

%%

prog:
| e = expr; EOF { e }
;

expr:
| OINK id = IDENT EQ e1 = expr MUD e2 = expr { print_endline "Matched OINK"; Oink (id, e1, e2) }
| id=IDENT { Ident id }
| s = STRING {String s}
| i = INT { Int i }
| x = FLOAT {Float x}
| TRUE {Bool true}
| FALSE {Bool false}

;


