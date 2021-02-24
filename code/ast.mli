
type operator = 
    Add | Sub | Mul | Div |
    Eql | Gre | Les | GreEql | LesEql | And | Or

type uni_operator = Not | Negate

type var_type = Int_ | Float_ | Str_ | Bool_ | Pint_ | Pix_ | Img_

type colors =
    Black | White | Red | Green | Blue | Cyan | Magenta | Yellow

type expr =
    Binop of expr * operator * expr
  | Uniop of uni_operator * expr
  | AssignOp of expr * expr
  | Int of int
  | Float of float
  | Bool of bool
  | Array of expr
  | Var of string
  | Expr of expr
  | Color of colors
  | Typeset of var_type * string
  | Binf of string * expr
  | ObjFunc of string * string

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
