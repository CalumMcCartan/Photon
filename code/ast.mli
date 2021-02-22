
type operator = 
    Add | Sub | Mul | Div |
    Eql | Gre | Les | GreEql | LesEql | And | Or |
    Semi

type expr =
    Binop of expr * operator * expr
  | AssignOp of string * expr
  | Int of int
  | Pint of int
  | Float of float
  | Var of string
  | Expr of expr

type stmts = 
    Repeated of stmts * stmts
  | Expr of expr


type fdel = 
    Fdel of string * string * stmts