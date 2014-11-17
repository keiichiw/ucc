open Format
exception Error
exception NotMatch
exception EvalError
exception TypeError of string
exception TODO of string

(*location*)
type loc = Lexing.position * Lexing.position

type name = Name of string
type ctype = TInt | TChar
type decl =
  | DVars of ctype * (name list) * loc
  | DFun of ctype * name * (ctype * name) list * stmt list * loc
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
  | EApp   of name * (expr list)
  | ELt    of expr * expr
  | EEq    of expr * expr
and value =
  | VInt of int
