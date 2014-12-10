(*location*)
type loc = Lexing.position * Lexing.position

type name = Name of string
type ctype =
  | TInt
  | TStruct of (name option) * ((dvar list) option)
  | TPtr of ctype (* pointer *)
  | TArray of ctype * int (* array *)
and def =
  | DefFun of ctype * name * (dvar list) * block * loc
  | DefVar of dvar
and block =
  | Block of dvar list * stmt list
and dvar =
  | DVar of ctype * name * (expr option)
  | DStruct of name * (dvar list)
and stmt =
  | SNil
  | SBlock of dvar list * stmt list
  | SWhile of expr * stmt
  | SDoWhile of stmt * expr
  | SFor of (expr option) * (expr option) * (expr option) * stmt
  | SIfElse of expr * stmt * stmt
  | SReturn of expr
  | SContinue
  | SBreak
  | SLabel of string * stmt
  | SGoto of string
  | SSwitch of expr * stmt
  | SCase of expr
  | SDefault
  | SExpr of expr
and expr =
  | EConst of ctype * value
  | EVar   of ctype * name
  | EComma of ctype * expr * expr
  | EAdd   of ctype * expr * expr
  | EShift of ctype * expr * expr
  | ESub   of ctype * expr * expr
  | ESubst of ctype * expr * expr
  | EApp   of ctype * name * (expr list)
  | ELe    of ctype * expr * expr
  | EEq    of ctype * expr * expr
  | ENeq   of ctype * expr * expr
  | EAddr  of ctype * expr
  | EPtr   of ctype * expr
  | ECond  of ctype * expr * expr * expr
  | EAnd   of ctype * expr * expr
  | EOr    of ctype * expr * expr
  | EDot   of ctype * expr * name
and value =
  | VInt of int
