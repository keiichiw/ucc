exception NotMatch
exception TODO of string

(*location*)
type loc = Lexing.position * Lexing.position

type name = Name of string
type size = int
type star_num = int
type ctype =
  | TInt
  | TStruct of (name option) * ((dvar list) option)
  | TPtr of ctype
  | TArray of ctype * int
  | TFun of ctype * (dvar list)
and def =
  | DefFun of dvar * block
  | DefVar of dvar
and dvar =
  | DVar of ctype * name * (expr option)
  | DStruct of name * (dvar list)
and block =
  | Block of (dvar list) * (stmt list)
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
  | EConst  of value
  | EVar    of name
  | EComma  of expr * expr
  | EAdd    of expr * expr
  | EShift  of expr * expr
  | ESub    of expr * expr
  | EAssign of expr * expr
  | EApp    of expr * (expr list)
  | ELe     of expr * expr
  | EEq     of expr * expr
  | ENeq    of expr * expr
  | EPtr    of expr
  | EAddr   of expr
  | EArray  of expr * expr
  | ECond   of expr * expr * expr
  | EAnd    of expr * expr
  | EOr     of expr * expr
  | EDot    of expr * name
and value =
  | VInt of int
