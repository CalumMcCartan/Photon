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
| '!' { NOT }

(* Data Types*)
| "int" { INT_ }
| "float" { FLOAT_ }
| "string" { STR_ }
| "bool" { BOOL_ }
| "Image" { IMG_ }
| "pint" { PINT_ }
| "pixel" { PIX_ }

(* Literals *)
| "true" { BOOL(true) }
| "false" { BOOL(false) }
| ['0'-'9']+ as lit { INT(int_of_string lit) }
| ['0'-'9']+'.'['0'-'9']* as lit { FLOAT(float_of_string lit) }
| ['a'-'z']+ as var { VAR(var) }

(* Other *)
| '.' { PERIOD }
| '=' { ASSIGN }
| ';' { SEMI }
| [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LCURL }
| '}' { RCURL }
| eof { EOF }
