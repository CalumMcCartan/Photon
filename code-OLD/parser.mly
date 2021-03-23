%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE
%token EQUAL GREATER GREATER_EQUAL LESS LESS_EQUAL AND OR NOT
%token EOF SEMI ASSIGN COLON
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> VAR STR
%token NULL
%token FDECL IF ELSE WHILE FOR
%token BLACK WHITE RED GREEN BLUE CYAN MAGENTA YELLOW
%token INT_ FLOAT_ STR_ BOOL_ PINT_ PIX_ IMG_ VOID_
%token RPAREN LPAREN RCURL LCURL PERIOD LSQR RSQR COMMA QUOTE

%left SEMI
%right ASSIGN
%left OR
%left AND
%left EQUAL GREATER GREATER_EQUAL LESS LESS_EQUAL
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT 

%start program
%type <Ast.fdel> program

%%

program:
  /* empty */              { NoFdel }
| stmt program             { NoFdel }
| fdel program             { $1 }

fdel:
| FDECL return_types VAR LPAREN vars RPAREN
    LCURL stmts RCURL       { Fdel($2, $3, $5, $8) }

stmts:
  /* empty */               { None }
| stmts stmt                { Repeated($1, $2) }

stmt:
| IF LPAREN expr RPAREN
    LCURL stmt RCURL
  ELSE LCURL stmt RCURL     { IfStmt($3, $6, $10) }
| WHILE LPAREN expr RPAREN
    LCURL stmt RCURL        { While($3, $6) }
| FOR LPAREN expr SEMI expr SEMI expr RPAREN
    LCURL stmt RCURL        { For($3, $5, $7, $10) }
| expr SEMI                 { Expr($1) }

vars:
  var COMMA vars           { "" }
| var                      { "" }

var:
  VAR                      { Var($1) }
| var LSQR expr RSQR       { $1 }

types:
| INT_                     { Int_ }
| FLOAT_                   { Float_ }
| STR_                     { Str_ }
| BOOL_                    { Bool_ }
| PINT_                    { Pint_ }
| PIX_                     { Pix_ }
| IMG_                     { Img_ }

return_types:
| types                    { $1 }
| VOID_                    { Void_ }

// Arrays
array:
| LSQR expr_list RSQR      { $2 }
| types array_size          { $2 }

array_size:
| array_size LSQR expr RSQR  { $3 }
| LSQR expr RSQR             { $2 }

expr_list:
| expr_list COMMA expr      { $1 }
| expr                      { $1 }

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
| MINUS expr                { Uniop(Negate, $2) }

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
| BOOL                      { Bool($1) }
| array                     { Array($1) }
| STR                       { Str($1) }
| NULL                      { Null }

// Declare variable
| INT_ VAR                  { Typeset(Int_, $2) }
| FLOAT_ VAR                { Typeset(Float_, $2) }
| STR_ VAR                  { Typeset(Str_, $2) }
| BOOL_ VAR                 { Typeset(Bool_, $2) }
| PINT_ VAR                 { Typeset(Pint_, $2) }
| PIX_ VAR                  { Typeset(Pix_, $2) }
| IMG_ VAR                  { Typeset(Img_, $2) }

| INT_ VAR ASSIGN expr      { Typeset(Int_, $2) }
| FLOAT_ VAR ASSIGN expr    { Typeset(Float_, $2) }
| STR_ VAR ASSIGN expr      { Typeset(Str_, $2) }
| BOOL_ VAR ASSIGN expr     { Typeset(Bool_, $2) }
| PINT_ VAR ASSIGN expr     { Typeset(Pint_, $2) }
| PIX_ VAR ASSIGN expr      { Typeset(Pix_, $2) }
| IMG_ VAR ASSIGN expr      { Typeset(Img_, $2) }

// Other
| var ASSIGN expr           { AssignOp($1, $3) }
| var                       { $1 }
| LPAREN expr RPAREN        { $2 }

// Built-In Functions
| VAR LPAREN expr_list RPAREN    { Binf($1, $3) }
| VAR PERIOD VAR LPAREN expr_list RPAREN   { ObjFunc($1, $3)}