
type operator = 
    Add | Sub | Mul | Div |
    Eql | Gre | Les | GreEql | LesEql | And | Or

type uni_operator = Not

type var_type = Int_ | Float_ | Str_ | Bool_ | Pint_ | Pix_ | Img_

type expr =
    Binop of expr * operator * expr
  | Uniop of uni_operator * expr
  | AssignOp of string * expr
  | Int of int
  | Float of float
  | Bool of bool
  | Var of string
  | Expr of expr
  | Typeset of var_type * string
  | Binf of string * expr
  | ObjFunc of string * string

type stmts = 
    Repeated of stmts * stmts
  | Expr of expr


type fdel = 
    Fdel of string * string * stmts

 