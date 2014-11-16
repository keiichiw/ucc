open Format
exception Error
exception NotMatch
exception EvalError
exception TypeError of string
exception TODO of string

type name = Name of string
type ctype = TInt | TChar
type defin =
  | DVar of ctype * name * expr
  | DFun of ctype * name * (ctype * name) list * stmt list
and stmt =
  | SNil
  | SWhile of expr * stmt list
  | SIf of expr * stmt list
  | SIfElse of expr * stmt list * stmt list
  | SReturn of expr
  | SExpr of expr
and expr =
  | EConst of value
  | EVar   of name
  | EAdd   of expr * expr
  | ESub   of expr * expr
  | EMod   of expr * expr
  | ELt    of expr * expr
  | EEq    of expr * expr
and value =
  | VInt of int
