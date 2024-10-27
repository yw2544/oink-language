%token <int> INT
%token <string> STRING
%token <float> FLOAT 
%token TRUE
%token FALSE
%token EOF


%start <Ast.expr> prog

%%

prog:
| e = expr; EOF { e }
;

expr:
| s = STRING {String s}
| i = INT { Int i }
| x = FLOAT {Float x}
| TRUE {Bool true}
| FALSE {Bool false}
;


