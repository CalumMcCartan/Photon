open Ast

type sexpr = var_type * sx
and sx =
    SBinop of sexpr * operator * sexpr
  | SUniop of uni_operator * sexpr
  | SAssignOp of sexpr * sexpr
  | SInt of int
  | SFloat of float
  | SBool of bool
  | SStr of string
  | SNull
  | SArray of sexpr
  | SVar of string
  | SExpr of sexpr
  | SColor of colors
  | STypeset of var_type * string
  | SBinf of string * sexpr
  | SObjFunc of string * string

type sstmt = 
  SExpr of sexpr
| SIfStmt of sexpr * sstmt * sstmt
| SWhile of sexpr * sstmt
| SFor of sexpr * sexpr * sexpr * sstmt

type sstmts =
| SRepeated of sstmts * sstmt
| SNone

type sfdel = 
    SFdel of var_type * string * string * sstmts
|   SNoFdel