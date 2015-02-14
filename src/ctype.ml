open Util
open Printf

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

let struct_env : (string * ctype) list list ref = ref []
let union_env  : (string * ctype) list list ref = ref []

let rev_table_struct : (int * string) list ref = ref []
let rev_table_union  : (int * string) list ref = ref []

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


let is_integral = function
  | TInt  | TShort  | TLong  | TChar
  | TUInt | TUShort | TULong | TUChar -> true
  | _ -> false

let is_unsigned = function
  | TUInt | TUShort | TULong | TUChar -> true
  | _ -> false

let is_arith t = is_integral t || t = TFloat

let is_pointer = function
  | TPtr _ -> true
  | _ -> false

let is_scalar t = is_arith t || is_pointer t

let is_funty = function
  | TFun _ -> true
  | _ -> false

let deref_pointer = function
  | TPtr ty -> ty
  | _ -> failwith "deref_pointer"


(* functions for fold-expression *)

let uint_of_int x =
  if x < 0 then x + 0x100000000 else x

let arith2fun ty = function
  | Add -> (+)
  | Sub -> (-)
  | Mul -> ( * )
  | Div ->
     if is_unsigned ty then
       fun x y -> uint_of_int x / uint_of_int y
     else (/)
  | Mod ->
     if is_unsigned ty then
       fun x y -> uint_of_int x mod uint_of_int y
     else (mod)
  | LShift -> (lsl)
  | RShift ->
     if is_unsigned ty then
       fun x y -> (x land 0xffffffff) lsr y
     else (asr)
  | BitAnd -> (land)
  | BitXor -> (lxor)
  | BitOr  -> (lor)

let farith2fun = function
  | Add -> (+.)
  | Sub -> (-.)
  | Mul -> ( *. )
  | Div -> (/.)
  | _ -> failwith "farith2fun"

let rel2fun rel =
  let op =
    match rel with
    | Lt -> (<)
    | Le -> (<=)
    | Gt -> (>)
    | Ge -> (>=) in
  (fun a b -> if (op a b) then 1 else 0)

let urel2fun rel =
  let op =
    match rel with
    | Lt -> (<)
    | Le -> (<=)
    | Gt -> (>)
    | Ge -> (>=) in
  (fun a b -> if (op (uint_of_int a) (uint_of_int b)) then 1 else 0)

let eq2fun eq =
  let op =
    match eq with
    | Eq -> (=)
    | Ne -> (<>) in
  (fun a b -> if (op a b) then 1 else 0)

let unary2fun = function
  | Plus   -> (+) 0
  | Minus  -> (-) 0
  | BitNot -> (lnot)
  | LogNot -> (fun x -> if x=0 then 1 else 0)
  | _ -> failwith "unary2fun: PostInc/PostDec"


(* pretty-printing *)

let rec pp_struct id =
  try
    let s = List.assoc id !rev_table_struct in
    sprintf "struct %s" s
  with
    Not_found ->
      let m  = (List.nth !struct_env id) in
      let ms = String.concat "; "
        (List.map (fun (_, ty) -> pp_type ty) m) in
      sprintf "struct {%s;}" ms
and pp_union id =
  try
    let s = List.assoc id !rev_table_struct in
    sprintf "union %s" s
  with
    Not_found ->
      let m  = (List.nth !union_env id) in
      let ms = String.concat "; "
        (List.map (fun (_, ty) -> pp_type ty) m) in
      sprintf "union {%s;}" ms
and pp_type ty =
  let rec go str = function
    | TInt    -> "int"   ^ str
    | TShort  -> "short" ^ str
    | TLong   -> "long"  ^ str
    | TChar   -> "char"  ^ str
    | TUInt   -> "unsigned" ^ str
    | TUShort -> "unsigned short" ^ str
    | TULong  -> "unsigned long"  ^ str
    | TUChar  -> "unsigned char"  ^ str
    | TFloat  -> "float" ^ str
    | TVoid   -> "void"  ^ str
    | TStruct id ->
      sprintf "%s%s" (pp_struct id) str
    | TUnion id ->
      sprintf "%s%s" (pp_union id) str
    | TPtr ty  ->
      let s = match ty with
        | TArray _ | TFun _ ->
          sprintf "(*%s)" str
        | TPtr _ ->
          sprintf "*%s" str
        | _ ->
          sprintf " *%s" str in
      go s ty
    | TArray (ty, sz) ->
      go (str ^ "[]") ty
    | TFun (ty, args)->
      let a = String.concat ", " (List.map (go "") args) in
      go (sprintf "%s(%s)" str a) ty
  in
  go "" ty
