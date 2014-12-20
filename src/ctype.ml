exception TODO of string

type size = int

type name =
  | Name of string

type ctype =
  | TInt
  | TShort
  | TLong
  | TUnsigned
  | TChar
  | TVoid
  | TStruct of int
  | TUnion of int
  | TPtr of ctype
  | TArray of ctype * int
  | TFun of ctype * (ctype list)

type linkage =
  | Static
  | Extern
  | NoLink

let is_funty = function
  | TFun _ -> true
  | _ -> false

(* operator definitions *)

type arith_bin =
  | Add | Sub
  | Mul | Div | Mod
  | LShift | RShift
  | BitAnd | BitXor | BitOr

type logical_bin = LogAnd | LogOr

type rel_bin = Lt | Le | Gt | Ge

type eq_bin = Eq | Ne

type unary = Plus | Minus | BitNot | LogNot | PostInc | PostDec

type inc = Inc | Dec
