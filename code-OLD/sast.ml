
type sexpr = var_type * sx

type sx =
    Binop of sexpr * operator * sexpr
  | Uniop of uni_operator * sexpr
  | AssignOp of sexpr * sespr
  | Int of int
  | Float of float
  | Bool of bool
  | Str of string
  | Null
  | Array of sexpr
  | Var of string
  | Expr of sexpr
  | Color of colors
  | Typeset of var_type * string
  | Binf of string * sexpr
  | ObjFunc of string * string

type sstmt = 
  Expr of sexpr
| IfStmt of sexpr * sstmt * sstmt
| While of sexpr * sstmt
| For of sexpr * sexpr * sexpr * sstmt

type sstmts =
| Repeated of sstmts * sstmt
| None

type sfdel = 
    Fdel of var_type * string * string * sstmts
|   NoFdel