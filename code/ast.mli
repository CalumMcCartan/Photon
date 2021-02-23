
type operator = 
    Add | Sub | Mul | Div |
    Eql | Gre | Les | GreEql | LesEql | And | Or |
    Semi

type uni_operator = Not

type expr =
    Binop of expr * operator * expr
  | Uniop of uni_operator * expr
  | AssignOp of string * expr
  | Int of int
  | Pint of int
  | Float of float
  | Bool of bool
  | Var of string
  | Expr of expr

type stmts = 
    Repeated of stmts * stmts
  | Expr of expr


type fdel = 
    Fdel of string * string * stmts