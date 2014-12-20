open Ctype
type name = Name of string
type struct_id = int
type size = int

type def =
  | DefFun of decl * (decl list) * stmt
  | DefVar of decl
and decl =
  | Decl of linkage * ctype * name * (expr list)
and stmt =
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
and arith_bin =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | LShift
  | RShift
  | BitAnd (* & *)
  | BitXor (* ^ *)
  | BitOr  (* | *)
and logical_bin =
  | And (* && *)
  | Or  (* || *)
and rel_bin =
  | Lt
  | Le
  | Gt
  | Ge
and eq_bin =
  | Eq
  | Ne
and unary =
  | Plus
  | Minus
  | BitNot (* ~ *)
  | LogNot (* ! *)
  | PostInc
  | PostDec
and inc =
  | Inc
  | Dec
and expr =
  | EArith  of ctype * arith_bin   * expr * expr
  | EPAdd   of ctype * expr * expr
  | EPDiff  of ctype * expr * expr
  | ERel    of ctype * rel_bin     * expr * expr
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
and value =
  | VInt of int
  | VStr of int list
