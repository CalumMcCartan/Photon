%{ open Ast %}

%token EXPR_SEP ASSIGN PLUS MINUS TIMES DIVIDE EOF
%token <int> LITERAL
%token <string> VAR

%left EXPR_SEP
%right ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE

%start expr
%type <Ast.expr> expr

%%

expr:
  VAR  ASSIGN   expr { AssignOp($1, $3) }
| expr EXPR_SEP expr { Binop($1, Sep, $3) }
| expr PLUS     expr { Binop($1, Add, $3) }
| expr MINUS    expr { Binop($1, Sub, $3) }
| expr TIMES    expr { Binop($1, Mul, $3) }
| expr DIVIDE   expr { Binop($1, Div, $3) }
| LITERAL            { Lit($1) }
| VAR                { Var($1) }
