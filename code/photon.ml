open Ast

let print_list = function
  list -> print_endline(String.concat " " list)

let rec eval = function 
| Int(x) -> print_list ["Int"; string_of_int x] ; x
| Float(x) -> print_list ["Float"; string_of_float x] ; 0
| Pint(x) -> print_list ["Pint"; string_of_int x] ; x
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
| AssignOp(var, expr) ->
  let value = eval expr in 
  print_list ["Assign"; var; "to"; string_of_int value] ;
  value


let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.tokenize lexbuf in
  let result = eval expr in
  print_endline (string_of_int result)
