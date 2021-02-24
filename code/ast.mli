
type operator = 
    Add | Sub | Mul | Div |
    Eql | Gre | Les | GreEql | LesEql | And | Or |
    Semi

type colors =
    Black | White | Red | Green | Blue | Cyan | Magenta | Yellow

type expr =
    Binop of expr * operator * expr
  | AssignOp of string * expr
  | Int of int
  | Pint of int
  | Float of float
  | Var of string
  | Expr of expr
  | Color of colors

type stmt = 
  Expr of expr
| IfStmt of expr * stmt * stmt
| While of expr * stmt
| For of expr * expr * expr * stmt

type stmts =
  Single of stmt
| Repeated of stmts * stmt

type fdel = 
    Fdel of string * string * string * stmts