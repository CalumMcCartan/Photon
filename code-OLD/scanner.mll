{ open Parser }


rule tokenize = parse
  
(* Math Operators *)
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }

(* Bool Operators *)
| "==" { EQUAL }
| '>' { GREATER }
| ">=" { GREATER_EQUAL }
| '<' { LESS }
| "<=" { LESS_EQUAL }
| "&" { AND }
| "||" { OR }
 
(* Functions, If, Loops *)
| "func" { FDECL }
| "if" { IF }
| "else" { ELSE }
| "while" { WHILE }
| "for" { FOR }

(* Color Keywords *)
| "_black" { BLACK }
| "_white" { WHITE }
| "_red" { RED }
| "_green" { GREEN }
| "_blue" { BLUE }
| "_cyan" { CYAN }
| "_magenta" { MAGENTA }
| "_yellow" { YELLOW }
| '!' { NOT }

(* Data Types*)
| "int" { INT_ }
| "float" { FLOAT_ }
| "string" { STR_ }
| "bool" { BOOL_ }
| "Image" { IMG_ }
| "pint" { PINT_ }
| "pixel" { PIX_ }
| "void" { VOID_ }

(* Other *)
| '#' { comment lexbuf }
| '"' { QUOTE }
| '.' { PERIOD }
| ',' { COMMA }
| '=' { ASSIGN }
| ';' { SEMI }
| ':' { COLON }
| [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LCURL }
| '}' { RCURL }
| '[' { LSQR }
| ']' { RSQR }
| eof { EOF }

(* Literals *)
| "null" { NULL }
| "true" { BOOL(true) }
| "false" { BOOL(false) }
| ['0'-'9']+ as lit { INT(int_of_string lit) }
| ['0'-'9']+'.'['0'-'9']* as lit { FLOAT(float_of_string lit) }
| ['a'-'z' 'A'-'Z']+['a'-'z' 'A'-'Z' '0'-'9' '_']* as var { VAR(var) }
| '"'[' '-'!' '#'-'~']+'"' as lit { STR(lit) }

(* Comments *)
and comment = parse
| '\n' { tokenize lexbuf }
| eof  { tokenize lexbuf }
| _    { comment lexbuf }
