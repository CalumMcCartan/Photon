
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
  | Int_ of string
  | Float_ of string
  | Str_ of string
  | Bool_ of string
  | Arr_ of string
  | Pint_ of string
  | Pix_ of string
  | Img_ of string
  | Typeset of string * string
  | Binf of string * expr


type stmts = 
    Repeated of stmts * stmts
  | Expr of expr


type fdel = 
    Fdel of string * string * stmts

 