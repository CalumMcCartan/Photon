open Ast

let print_list = function
  list -> print_endline(String.concat " " list)

let stmt_to_string = function
  stmt -> ""

let stmts_to_string = function
  stmts -> ""

let rec eval = function 
| Int(x) -> print_list ["Int"; string_of_int x] ; x
| Float(x) -> print_list ["Float"; string_of_float x] ; 0
| Bool(x) -> print_list ["Bool"; string_of_bool x] ; 0
| Var(x) -> print_list ["Var"; x] ; 0
| Binop(e1, op, e2) ->
  let v1 = eval e1 in
  let v2 = eval e2 in
  let opType = 
    (match op with
      Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Eql -> "=="
    | Gre -> ">"
    | GreEql -> ">="
    | Les -> "<"
    | LesEql -> "<="
    | And -> "&"
    | Or -> "||"
    ) in
  print_list ["Binop"; string_of_int v1; opType; string_of_int v2] ;
  0
| Uniop(op, expr) ->
  let value = eval expr in
  let opType = (match op with Not -> "!") in
  print_list ["Uninop"; opType; string_of_int value] ;
  0
| AssignOp(var, expr) ->
  let value = eval expr in 
  print_list ["Assign"; var; "to"; string_of_int value] ;
  value
| Typeset (var_type, var) ->
  let type_name = 
    (match var_type with
      Int_ -> "int"
    | Float_ -> "float"
    | Str_ -> "string"
    | Bool_ -> "bool"
    | Img_ -> "Image"
    | Pint_ -> "pint"
    | Pix_ -> "pixel"
    ) in
  print_list ["Set type of"; var; "to"; type_name] ; 0
| Expr(expr) ->
  let value = eval expr in 
  print_list ["Expr"; string_of_int value] ; value
| Color(c) ->
  let colorval = 
    (match c with
      Black -> "Black"
    | White -> "White"
    | Red -> "Red"
    | Green -> "Green"
    | Blue -> "Blue"
    | Cyan -> "Cyan"
    | Magenta -> "Magenta"
    | Yellow -> "Yellow") in
  print_list ["Color"; colorval]; 0
| Binf(funcname, expr) ->
    let value = eval expr in
    print_list [funcname; string_of_int value] ; 0
| ObjFunc(expr, funcname) ->
    print_list [funcname; expr] ; 0
    

let rec read = function
| Expr(expr) ->
  let value = eval expr in 
  print_list ["Expr"; string_of_int value] ; value
| IfStmt(expr, stmt1, stmt2) ->
  print_endline "If Start" ;
  let value = eval expr in 
  let result1 = read stmt1 in
  print_list ["If End"; string_of_int value; stmt_to_string result1] ;
  let result2 = read stmt2 in
  print_list ["Else End"; stmt_to_string result2]; result2
| While(expr, stmt) ->
  print_endline "While Start" ;
  let value = eval expr in 
  let result1 = read stmt in
  print_list ["While End"; string_of_int value; stmt_to_string result1] ; result1
| For(expr1, expr2, expr3, stmt) ->
    print_endline "For Start" ;
    let value1 = eval expr1 in
    let value2 = eval expr2 in
    let value3 = eval expr3 in  
    let result1 = read stmt in
    print_list ["For End"; string_of_int value1; string_of_int value2;
    string_of_int value3; stmt_to_string result1] ; result1

let rec reads = function
| Single(stmt) ->
  let result1 = read stmt in
  print_list ["Statement"; stmt_to_string result1]; result1
| Repeated(stmts, stmt) ->
  let result1 = reads stmts in
  let result2 = read stmt in
  print_list ["Statements"; stmt_to_string result2]; result2

let rec fdel = function
| Fdel(typ, name, input, stmts) ->
  print_list ["Function Start"; typ; name; input] ;
  let result = reads stmts in
  print_list ["Function End"; typ; name; input; stmts_to_string result]; None

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let func = Parser.fdel Scanner.tokenize lexbuf in
  let _ = fdel func in 
  print_endline("Done")