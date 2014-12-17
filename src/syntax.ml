exception NotMatch
exception TODO of string

type name = Name of string
type size = int
type star_num = int
type id = int
type ctype =
  | TInt
  | TUnsigned
  | TStruct of id
  | TPtr of ctype
  | TArray of ctype * int
  | TFun of ctype * (dvar list)
and def =
  | DefFun of dvar * stmt
  | DefVar of dvar
and dvar =
  | DVar of ctype * name * (expr option)
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
and expr =
  | EConst  of value
  | EVar    of name
  | EComma  of expr * expr
  | EAssign of expr * expr
  | EUnary  of unary * expr
  | EArith  of arith_bin * expr * expr
  | ERel    of rel_bin   * expr * expr
  | EEq     of eq_bin    * expr * expr
  | ELog    of logical_bin * expr * expr
  | ECall   of expr * (expr list)
  | EPtr    of expr
  | EAddr   of expr
  | ECond   of expr * expr * expr
  | EDot    of expr * name
  | EArray  of expr * expr
  | ECast   of ctype * expr
and value =
  | VInt of int
let struct_table : (string * int) list ref = ref [];;
let struct_env : (int * (dvar list)) list ref = ref [];;
