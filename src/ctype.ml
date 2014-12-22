open Util

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

let struct_env : (string * ctype) list list ref = ref []
let union_env  : (string * ctype) list list ref = ref []

let rec sizeof = function
  | TInt | TShort | TLong | TUnsigned | TChar | TPtr _ -> 1
  | TVoid -> failwith "sizeof void"
  | TStruct s_id -> sum_of (List.map (snd >> sizeof) (List.nth !struct_env s_id))
  | TUnion u_id -> max_of (List.map (snd >> sizeof) (List.nth !union_env u_id))
  | TArray (ty, sz) -> (sizeof ty) * sz
  | TFun _ -> failwith "sizeof function"


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
