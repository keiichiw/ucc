open Ctype
exception NotMatch
exception TODO of string

type name = Name of string
type size = int
type id = int

type value =
  | VInt of int
  | VStr of int list

type expr =
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
  | ECast   of ctype * expr
  | ESizeof of ctype

type init =
  | IScal of expr
  | IVect of init list

type decl =
  | Decl of linkage * ctype * name * (init option)

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

let struct_table : (string * int) list ref = ref []
let struct_env : (int * (decl list)) list ref = ref []
