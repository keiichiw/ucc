open Util

exception TODO of string

type size = int

type name = string

type ctype =
  | TInt  | TShort  | TLong  | TChar
  | TUInt | TUShort | TULong | TUChar
  | TFloat
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
  | TInt  | TShort  | TLong  | TChar
  | TUInt | TUShort | TULong | TUChar -> 1
  | TFloat | TPtr _ -> 1
  | TStruct s_id ->
     s_id |> List.nth !struct_env
          |> List.map (snd >> sizeof)
          |> Util.sum_of
  | TUnion u_id ->
     u_id |> List.nth !union_env
          |> List.map (snd >> sizeof)
          |> Util.max_of
  | TArray (ty, sz) -> (sizeof ty) * sz
  | TFun _ -> failwith "sizeof function"
  | TVoid -> failwith "sizeof void"


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

(* functions for fold-expression *)
let arith2fun = function
  | Add -> (+)
  | Sub -> (-)
  | Mul -> ( * )
  | Div -> (/)
  | Mod -> (mod)
  | LShift -> (lsl)
  | RShift -> (lsr)
  | BitAnd -> (land)
  | BitXor -> (lxor)
  | BitOr  -> (lor)

let rel2fun rel =
  let op =
    match rel with
    | Lt -> (<)
    | Le -> (<=)
    | Gt -> (>)
    | Ge -> (>=) in
  (fun a b -> if (op a b) then 1 else 0)

let eq2fun eq =
  let op =
    match eq with
    | Eq -> (=)
    | Ne -> (!=) in
  (fun a b -> if (op a b) then 1 else 0)

let unary2fun = function
  | Plus   -> (+) 0
  | Minus  -> (-) 0
  | BitNot -> (lnot)
  | LogNot -> (fun x -> if x=0 then 1 else 0)
  | _ -> failwith "unary2fun: PostInc/PostDec"

let is_integral = function
  | TInt  | TShort  | TLong  | TChar
  | TUInt | TUShort | TULong | TUChar -> true
  | _ -> false

let is_unsigned = function
  | TUInt | TUShort | TULong | TUChar -> true
  | _ -> false

let is_num t = is_integral t || t = TFloat

let is_pointer = function
  | TPtr _ -> true
  | _ -> false
