open Format
exception NotMatch
exception TODO of string

(*location*)
type loc = Lexing.position * Lexing.position

type name = Name of string
type size = int
type dectype =
  | DeclIdent of name
  | DeclArray of dectype * size
type ctype =
  | TInt
  | TPtr of ctype (* pointer of type *)
type def =
  | DefFun of ctype * name * (dvar list) * block * loc
and block =
  | Block of dvar list * stmt list
and dvar =
  | DVar of ctype * name
  | DArray of ctype * name * size
and stmt =
  | SNil
  | SWhile of expr * block
  | SDoWhile of block * expr
  | SFor of (expr option) * (expr option) * (expr option) * block
  | SIfElse of expr * block * block
  | SReturn of expr
  | SContinue
  | SBreak
  | SExpr of expr
and expr =
  | EConst of value
  | EVar   of name
  | EComma of expr * expr
  | EAdd   of expr * expr
  | EShift of expr * expr
  | ESub   of expr * expr
  | ESubst of expr * expr
  | EApp   of name * (expr list)
  | ELe    of expr * expr
  | EEq    of expr * expr
  | ENeq    of expr * expr
  | EPtr   of expr
  | EAddr   of expr
and value =
  | VInt of int
