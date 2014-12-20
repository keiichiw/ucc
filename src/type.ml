open Ctype

type value =
  | VInt of int
  | VStr of int list

type expr =
  | EArith  of ctype * arith_bin   * expr * expr
  | EPAdd   of ctype * expr * expr
  | EPDiff  of ctype * expr * expr
  | ERel    of ctype * rel_bin     * expr * expr
  | EURel   of ctype * rel_bin     * expr * expr
  | EEq     of ctype * eq_bin      * expr * expr
  | ELog    of ctype * logical_bin * expr * expr
  | EUnary  of ctype * unary * expr
  | EPPost  of ctype * inc * expr
  | EConst  of ctype * value
  | EVar    of ctype * name
  | EComma  of ctype * expr * expr
  | EAssign of ctype * expr * expr
  | ECall   of ctype * expr * (expr list)
  | EAddr   of ctype * expr
  | EPtr    of ctype * expr
  | ECond   of ctype * expr * expr * expr
  | EDot    of ctype * expr * name
  | ECast   of ctype * ctype * expr

type decl =
  | Decl of linkage * ctype * name * (expr list)

type stmt =
  | SNil
  | SBlock of decl list * stmt list
  | SWhile of expr * stmt
  | SDoWhile of stmt * expr
  | SFor of (expr option) * (expr option) * (expr option) * stmt
  | SIfElse of expr * stmt * stmt
  | SReturn of expr option
  | SContinue
  | SBreak
  | SLabel of string * stmt
  | SGoto of string
  | SSwitch of expr * stmt
  | SCase of int
  | SDefault
  | SExpr of expr

type def =
  | DefFun of decl * (decl list) * stmt
  | DefVar of decl
