open Format
exception NotMatch
exception TODO of string

(*location*)
type loc = Lexing.position * Lexing.position

type name = Name of string
type ctype = TInt | TChar
type decl =
  | DVars of ctype * (declarator list) * loc
  | DFun of ctype * name * (ctype * name) list * block * loc
and declarator =
  | DeclIdent of name
  | DeclFProto of declarator * (ctype list)
and block =
  | Block of svar list * stmt list
and svar =
  | SVar of ctype * name
and stmt =
  | SNil
  | SWhile of expr * block
  | SFor of (expr option) * (expr option) * (expr option) * block
  | SIfElse of expr * block * block
  | SReturn of expr
  | SExpr of expr
and expr =
  | EConst of value
  | EVar   of name
  | EComma of expr * expr
  | EAdd   of expr * expr
  | ESub   of expr * expr
  | ESubst of name * expr
  | EMod   of expr * expr
  | EApp   of name * (expr list)
  | ELt    of expr * expr
  | EEq    of expr * expr
  | ENeq    of expr * expr
and value =
  | VInt of int
