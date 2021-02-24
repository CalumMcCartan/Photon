{ open Parser }

let digit = ['0'-'9']
let float = digit+'.'digit*
let pint =  ['0''1']?digit?digit | '2'['0'-'4']digit | "25"['0'-'5']

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
| "int" as dtype {INT_(dtype)}
| "float" as dtype {FLOAT_(dtype)}
| "string" as dtype {STR_(dtype)}
| "bool" as dtype {BOOL_(dtype)}
| "[]" as dtype {ARR_(dtype)}
| "Image" as dtype {IMG_(dtype)}
| "pint" as dtype {PINT_(dtype)}
| "pixel" as dtype {PIX_(dtype)}

(* Literals *)
| "true" { BOOL(true) }
| "false" { BOOL(false) }
| digit+ as lit { INT(int_of_string lit) }
| pint as lit { PINT(int_of_string lit) }
| float as lit { FLOAT(float_of_string lit) }
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
