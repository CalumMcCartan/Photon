%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE
%token EQUAL GREATER GREATER_EQUAL LESS LESS_EQUAL AND OR NOT
%token EOF SEMI ASSIGN
%token <int> INT
%token <int> PINT
%token <float> FLOAT
%token <bool> BOOL
%token <string> VAR
%token <string> LOAD FLIP ROTATE SAVE MIN MAX PRINT DESTROY
%token RPAREN LPAREN RCURL LCURL PERIOD

%left SEMI
%right ASSIGN
%left OR
%left AND
%left EQUAL GREATER GREATER_EQUAL LESS LESS_EQUAL
%left PLUS MINUS
%left TIMES DIVIDE
%left NOT

%start fdel
%type <Ast.fdel> fdel

%%


fdel:
| VAR LPAREN VAR RPAREN
    LCURL stmts RCURL       { Fdel($1, $3, $6) }

stmts:
| stmts stmts               { Repeated($1, $2)}
| expr                      { Expr($1) }

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
| NOT expr                  { Uniop(Not, $2) }

// Literals
| INT                       { Int($1) }
| FLOAT                     { Float($1) }
| PINT                      { Pint($1) }
| BOOL                      { Bool($1) }

// Other
| VAR ASSIGN expr           { AssignOp($1, $3) }
| VAR                       { Var($1) }
| expr SEMI expr            { Binop($1, Semi, $3) }
| LPAREN expr RPAREN        { $2 }

// Built-In Functions
| LOAD LPAREN expr RPAREN    { BINF(Load, $3) }
| expr PERIOD FLIP           { BINF(Flip, $1) }
| expr PERIOD ROTATE         { BINF(Rotate, $1)}
| expr PERIOD SAVE           { BINF(Save, $1)}
| PRINT LPAREN expr RPAREN   { BINF(Print, $3) }
| MIN LPAREN expr RPAREN     { BINF(Min, $3) }
| MAX LPAREN expr RPAREN     { BINF(Max, $3) }
| DESTROY LPAREN expr RPAREN { BINF(Destroy, $3)}
