exception NotMatch
exception TODO of string

(*location*)
type loc = Lexing.position * Lexing.position

type name = Name of string
type size = int
type ctype =
  | TInt
  | TStruct of (name option) * ((dvar list) option)
  | TPtr of ctype (* pointer *)
  | TArray of ctype (* array *)
and def =
  | DefFun of ctype * name * (dvar list) * block * loc
  | DefVar of dvar
and block =
  | Block of dvar list * stmt list
and dectype =
  | DeclIdent  of name * (expr option)
  | DeclArray  of dectype * size
and dvar =
  | DVar of ctype * name * (expr option)
  | DArray of ctype * name * size
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
  | ENeq   of expr * expr
  | EPtr   of expr
  | EAddr  of expr
  | EArray of expr * expr
  | ECond  of expr * expr * expr
  | EAnd   of expr * expr
  | EOr    of expr * expr
  | EDot   of expr * name
and value =
  | VInt of int
