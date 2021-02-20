%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE
%token EQUAL GREATER GREATER_EQUAL LESS LESS_EQUAL AND OR
%token EOF SEMI ASSIGN
%token <int> INT
%token <int> PINT
%token <float> FLOAT
%token <string> VAR

%left SEMI
%right ASSIGN
%left OR
%left AND
%left EQUAL GREATER GREATER_EQUAL LESS LESS_EQUAL
%left PLUS MINUS
%left TIMES DIVIDE

%start expr
%type <Ast.expr> expr

%%

expr:
// Math Operators
| expr PLUS expr            { Binop($1, Add, $3) }
| expr MINUS expr           { Binop($1, Sub, $3) }
| expr TIMES expr           { Binop($1, Mul, $3) }
| expr DIVIDE expr          { Binop($1, Div, $3) }

// Bool Operators
| expr EQUAL expr           { Binop($1, Eql, $3) }
| expr GREATER expr         { Binop($1, Gre, $3) }
| expr GREATER_EQUAL expr   { Binop($1, GreEql, $3) }
| expr LESS expr            { Binop($1, Les, $3) }
| expr LESS_EQUAL expr      { Binop($1, LesEql, $3) }
| expr AND expr             { Binop($1, And, $3) }
| expr OR expr              { Binop($1, Or, $3) }

// Literals
| INT                       { Int($1) }
| FLOAT                     { Float($1) }
| PINT                      { Pint($1) }

// Other
| VAR ASSIGN expr           { AssignOp($1, $3) }
| VAR                       { Var($1) }
| expr SEMI expr            { Binop($1, Semi, $3) }
