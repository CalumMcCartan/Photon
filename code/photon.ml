open Ast

let print_list = function
  list -> print_endline(String.concat " " list)

let stmt_to_string = function
  stmts -> ""

let rec eval = function 
| Int(x) -> print_list ["Int"; string_of_int x] ; x
| Float(x) -> print_list ["Float"; string_of_float x] ; 0
| Bool(x) -> print_list ["Bool"; string_of_bool x] ; 0
| Pint(x) -> print_list ["Pint"; string_of_int x] ; x
| Var(x) -> print_list ["Var"; x] ; 0
| Int_(x) -> print_list ["Int_"; x] ; 0
| Float_(x) -> print_list ["Float_"; x] ; 0
| Str_(x) -> print_list ["Str_"; x] ; 0
| Bool_(x) -> print_list ["Bool_"; x] ; 0
| Arr_(x) -> print_list ["Arr_"; x] ; 0
| Img_(x) -> print_list ["Img_"; x]; 0
| Pint_(x) -> print_list ["Pint_"; x]; 0
| Pix_(x) -> print_list ["Pix_"; x]; 0

| Binop(e1, op, e2) ->
  let v1 = eval e1 in
  let v2 = eval e2 in
  let opType = 
    (match op with
      Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Semi -> ";"
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
| Typeset (expr, var) ->
  print_list ["Set Type"; var; "to"; expr]; 0
| Expr(expr) ->
  let value = eval expr in 
  print_list ["Expr"; string_of_int value] ; 0
| Binf(funcname, expr) ->
    let value = eval expr in
    print_list [funcname; string_of_int value] ; 0
    
    

let rec read = function
| Repeated(stmt1, stmt2) ->
  let result1 = read stmt1 in
  let result2 = read stmt1 in
  print_list ["Statements"; stmt_to_string result1; stmt_to_string result2]; result1
| Expr(expr) ->
  let value = eval expr in 
  print_list ["Expr"; string_of_int value] ; expr

let rec fdel = function
| Fdel(name, typ, stmt) ->
  print_list ["Function Start"; name; typ] ;
  let result = read stmt in
  print_list ["Function End"; name; typ; stmt_to_string result]; None

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let func = Parser.fdel Scanner.tokenize lexbuf in
  let result = fdel func in 
  print_endline("Done")