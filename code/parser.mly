%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE
%token EQUAL GREATER GREATER_EQUAL LESS LESS_EQUAL AND OR NOT
%token EOF SEMI ASSIGN
%token <int> INT
%token <int> PINT
%token <float> FLOAT
%token <bool> BOOL
%token <string> VAR
%token <string> INT_ 
%token <string> FLOAT_ 
%token <string> STR_ 
%token <string> BOOL_ 
%token <string> ARR_ 
%token <string> PINT_ 
%token <string> PIX_ 
%token <string> IMG_

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

stmt:
| expr SEMI                { Expr($1) }

stmts:
| stmts stmt               { Repeated($1, $2)}
| stmt                     { $1 }

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

//Date Type
| INT_ VAR                  {Typeset($1,$2)}
| FLOAT_ VAR                {Typeset($1,$2)}
| STR_ VAR                  {Typeset($1,$2)}
| BOOL_ VAR                 {Typeset($1,$2)}
| ARR_ VAR                  {Typeset($1,$2)}
| PINT_ VAR                 {Typeset($1,$2)}
| PIX_ VAR                  {Typeset($1,$2)}
| IMG_ VAR                  {Typeset($1,$2)}

// Other
| VAR ASSIGN expr           { AssignOp($1, $3) }
| VAR                       { Var($1) }
| LPAREN expr RPAREN        { $2 }

// Built-In Functions
| VAR LPAREN expr RPAREN    { Binf($1, $3) }
| VAR PERIOD VAR         { ObjFunc($1, $3)}