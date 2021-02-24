%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE
%token EQUAL GREATER GREATER_EQUAL LESS LESS_EQUAL AND OR
%token EOF SEMI ASSIGN COLON
%token <int> INT
%token <int> PINT
%token <float> FLOAT
%token <string> VAR
%token RPAREN LPAREN RCURL LCURL
%token FDECL IF ELSE WHILE FOR
%token BLACK WHITE RED GREEN BLUE CYAN MAGENTA YELLOW

%left SEMI
%right ASSIGN
%left OR
%left AND
%left EQUAL GREATER GREATER_EQUAL LESS LESS_EQUAL
%left PLUS MINUS
%left TIMES DIVIDE

%start fdel
%type <Ast.fdel> fdel

%%

fdel:
| FDECL VAR VAR LPAREN VAR RPAREN
    LCURL stmts RCURL       { Fdel($2, $3, $5, $8) }

stmts:
  /* empty */
| stmt                      { Single($1) }
| stmts stmt                { Repeated($1, $2) }


stmt:
| IF LPAREN expr RPAREN
    LCURL stmt RCURL
  ELSE LCURL stmt RCURL     { IfStmt($3, $6, $10) }
| WHILE LPAREN expr RPAREN
    LCURL stmt RCURL        { While($3, $6) }
| FOR LPAREN expr COLON expr COLON expr RPAREN
    LCURL stmt RCURL        { For($3, $5, $7, $10) }
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

// Color Keywords
| BLACK                     { Color(Black) }
| WHITE                     { Color(White) }
| RED                       { Color(Red) }
| GREEN                     { Color(Green) }
| BLUE                      { Color(Blue) }
| CYAN                      { Color(Cyan) }
| MAGENTA                   { Color(Magenta) }
| YELLOW                    { Color(Yellow) }

// Literals
| INT                       { Int($1) }
| FLOAT                     { Float($1) }
| PINT                      { Pint($1) }

// Other
| VAR ASSIGN expr           { AssignOp($1, $3) }
| VAR                       { Var($1) }
| expr SEMI expr            { Binop($1, Semi, $3) }

