type operator = Add | Sub | Mul | Div | Sep

type expr =
    Binop of expr * operator * expr
  | AssignOp of string * expr
  | Lit of int
  | Var of string
