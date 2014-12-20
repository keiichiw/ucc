open Ctype
open Format

exception TypingError of string
exception TODO of string
exception Unreachable

let venv_ref : (string * ctype) list ref = ref [];;

(* This is initialized in main *)
let senv_ref : (int * ((string * ctype) list)) list ref = ref [];;

let push_stack x env =
  env := x::!env
let resolve_var_type nm =
  let rec go nm  = function
    | [] -> raise (TypingError (sprintf "variable not found: %s" nm))
    | (s, ty)::_ when s=nm -> ty
    | _ :: xs -> go nm xs in
  go nm !venv_ref
let resolve_member_type stct mem_name =
  match stct with
  | TStruct s_id ->
     let dvs = List.assoc s_id !senv_ref in
     List.assoc mem_name dvs
  | _ -> raise Unreachable

let sp_ref = ref 0
let sp_max = ref 0

let enter_block size =
  sp_ref := !sp_ref + size;
  sp_max := max !sp_ref !sp_max
let leave_block size =
  sp_ref := !sp_ref - size

let stack_info = ref []

let append_info x =
  stack_info := x :: !stack_info

let sum = List.fold_left (+) 0

let typeof = function
  | Type.EArith  (t, _, _, _) -> t
  | Type.ERel    (t, _, _, _) -> t
  | Type.EPAdd   (t, _, _) -> t
  | Type.EPDiff  (t, _, _) -> t
  | Type.EEq     (t, _, _, _) -> t
  | Type.ELog    (t, _, _, _) -> t
  | Type.EUnary  (t, _, _) -> t
  | Type.EPPost  (t, _, _) -> t
  | Type.EConst  (t, _) -> t
  | Type.EVar    (t, _) -> t
  | Type.EComma  (t, _, _) -> t
  | Type.EAssign (t, _, _) -> t
  | Type.ECall   (t, _, _) -> t
  | Type.EAddr   (t, _) -> t
  | Type.EPtr    (t, _) -> t
  | Type.ECond   (t, _, _, _) -> t
  | Type.EDot    (t, _, _) -> t
  | Type.ECast   (t, _, _) -> t

let rec sizeof = function
  | TInt
  | TShort
  | TLong
  | TUnsigned
  | TChar
  | TPtr _ -> 1
  | TArray (ty, sz) -> sz * (sizeof ty)
  | TFun _ -> raise (TypingError "sizeof function")
  | TStruct sid ->
     let sz fs = List.fold_left (fun num (_, ty) -> num + (sizeof ty)) 0 fs in
     let rec go = function
       | [] -> raise (TypingError (sprintf "struct %d not found" sid))
       | (s, ms)::_ when s = sid -> sz ms
       | _::zs -> go zs in
     go !senv_ref
  | TVoid ->
     raise (TypingError "sizeof void")

let is_integral = function
  | TInt | TShort | TLong | TUnsigned | TChar -> true
  | _ -> false

let int_conv = function
  | (TVoid, _) | (_, TVoid) -> raise (TypingError "int_conv: void")
  | (TLong, _) | (_, TLong) -> TLong
  | (TUnsigned, _) | (_, TUnsigned) -> TUnsigned
  | _ -> TInt

let initialize ty init =
  let scaler = function
    | Syntax.IVect ((Syntax.IVect _)::_) ->
      raise (TypingError "too many braces around scalar initializer")
    | Syntax.IVect [Syntax.IScal e] -> [e]
    | Syntax.IVect _ ->
      raise (TypingError "invalid scaler initializer")
    | Syntax.IScal e -> [e] in
  let rec compound ty init idx =
    match ty, init with
    | TStruct s_id, Syntax.IVect ilist ->
      let s = List.assoc s_id !senv_ref in
      if List.length s = idx then [], init
      else
        let l, rem  = inner (snd (List.nth s idx)) ty ilist in
        let r, tail = compound ty rem (idx + 1) in
        l @ r, tail
    | TArray (inner_ty, sz), Syntax.IVect ilist ->
      if sz = idx then [], init
      else
        let l, rem  = inner inner_ty ty ilist in
        let r, tail = compound ty rem (idx + 1) in
        l @ r, tail
    | TArray (TChar, _), Syntax.IScal (Syntax.EConst (Syntax.VStr str)) ->
      let f i = Syntax.IScal (Syntax.EConst (Syntax.VInt i)) in
      let ilist = Syntax.IVect (List.map f str) in
      compound ty ilist 0
    | _ -> raise (TypingError "requied initializer list")
  and inner inner_ty ty ilist =
    let i, is =
      if ilist = [] then
        Syntax.IScal (Syntax.EConst (Syntax.VInt 0)), []
      else
        List.hd ilist, List.tl ilist in
    match inner_ty, i with
    | TStruct _, Syntax.IVect _ | TArray _, Syntax.IVect _ ->
      let res, tail = compound inner_ty i 0 in
      if tail <> Syntax.IVect [] then
        raise (TypingError "initializer eccess elements");
      res, Syntax.IVect is
    | TStruct _, _ | TArray _, _ ->
      compound inner_ty (Syntax.IVect ilist) 0
    | _, _ -> scaler i, Syntax.IVect is in
  match init with
  | None -> []
  | Some init ->
    match ty with
    | TStruct _ | TArray _ ->
      let res, tail = compound ty init 0 in
      if tail <> Syntax.IVect [] then
        raise (TypingError "initializer eccess elements");
      res
    | _ -> scaler init

let rec ex e =
  let e = ex' e in
  let ty = typeof e in
  match ty with
  | TArray (ty, _) ->
     Type.EAddr (TPtr ty, e)
  | TFun _ ->
     Type.EAddr (TPtr ty, e)
  | _ ->
     e

and ex' = function
  | Syntax.EConst v ->
     let (ty, v) = match v with
       | Syntax.VInt i -> TInt, Type.VInt i
       | Syntax.VStr s -> TArray (TInt, List.length s), Type.VStr s in
     Type.EConst (ty, v)
  | Syntax.EVar (Syntax.Name n)->
     Type.EVar (resolve_var_type n, Type.Name n)
  | Syntax.EComma (e1, e2) ->
     let e = ex e2 in
     Type.EComma(typeof e, ex e1, e)
  | Syntax.EArith (op, e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     (match op with
      | Add ->
         (match (typeof ex1, typeof ex2) with
          | (TPtr ty, i) when is_integral i ->
             Type.EPAdd (TPtr ty, ex1, ex2)
          | (i, TPtr ty) when is_integral i ->
             Type.EPAdd (TPtr ty, ex2, ex1)
          | (ty1, ty2) when is_integral ty1 && is_integral ty2 ->
             let ty = int_conv (ty1,ty2) in
             Type.EArith (ty, Add, ex1, ex2)
          | _ -> raise (TypingError "EArith: add"))
      | Sub ->
         (match (typeof ex1, typeof ex2) with
          | (TPtr ty1, TPtr ty2) ->
             Type.EPDiff(TInt, ex1, ex2)
          | (TPtr ty1, i) when is_integral i ->
             let m_ex2 = ex (Syntax.EUnary(Minus, e2)) in
             assert (is_integral (typeof m_ex2));
             Type.EPAdd (TPtr ty1, ex1, m_ex2)
          | (ty1, ty2) when is_integral ty1 && is_integral ty2 ->
             let ty = int_conv (ty1,ty2) in
             Type.EArith (ty, Sub, ex1, ex2)
          | _ -> raise (TypingError "EArith: sub"))
      | _ ->
         (match (typeof ex1, typeof ex2) with
          | (t1, t2) when is_integral t1 && is_integral t2->
             let ty = int_conv (t1, t2) in
             Type.EArith (ty, op, ex1, ex2)
          | _ -> raise (TypingError "EArith")))
  | Syntax.ERel (op, e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     (match (typeof ex1, typeof ex2) with
      | (t1, t2) when is_integral t1 && is_integral t2 ->
         (match int_conv (t1, t2) with
          | TUnsigned ->
             raise (TypingError "relation: unsigned")
          | _ ->
             Type.ERel (TInt, op, ex1, ex2))
      | (TPtr _, TPtr _) ->
         raise (TypingError "relation: pointer")
      | _ ->
         raise (TypingError "relation"))
  | Syntax.EEq (op, e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     (match (typeof ex1, typeof ex2) with
      | (t1, t2) when is_integral t1 && is_integral t2 ->
         Type.EEq (TInt, op, ex1, ex2)
      | (TPtr _, TPtr _) ->
         Type.EEq (TInt, op, ex1, ex2)
      | (t, TPtr _) when is_integral t ->
         (match ex1 with
          | Type.EConst (_, Type.VInt 0) -> (* null pointer *)
             Type.EEq (TInt, op, ex1, ex2)
          | _ ->
             raise (TypingError "eq: pointer and non-zero integer"))
      | (TPtr _, t) when is_integral t ->
         (match ex2 with
          | Type.EConst (_, Type.VInt 0) -> (* null pointer *)
             Type.EEq (TInt, op, ex1, ex2)
          | _ ->
             raise (TypingError "eq: pointer and non-zero integer"))
      | _ ->
         raise (TypingError "eq: otherwise"))
  | Syntax.ELog (op, e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     (match (typeof ex1, typeof ex2) with
      | (t1, t2) when is_integral t1 && is_integral t2 ->
         (match int_conv (t1, t2) with
          | TUnsigned ->
             raise (TypingError "logical: unsigned")
          | ty -> (* long or int*)
             Type.ELog (TInt, op, ex1, ex2))
      | _ ->
         raise (TypingError "logical"))
  | Syntax.EUnary (op, e1) ->
     let ex1= ex e1 in
     (match (op, typeof ex1) with
      | (PostInc, TPtr t) ->
         Type.EPPost(TPtr t, Inc, ex1)
      | (PostDec, TPtr t) ->
         Type.EPPost(TPtr t, Dec, ex1)
      | (LogNot, _) (* ! *)
      | (_, TInt)
      | (_, TShort)
      | (_, TChar) ->
         Type.EUnary(TInt, op, ex1)
      | (_, TLong) ->
         Type.EUnary(TLong, op, ex1)
      | (_, TUnsigned) ->
         Type.EUnary(TUnsigned, op, ex1)
      | _ ->
         raise (TypingError "unary"))
  | Syntax.EAssign (e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     Type.EAssign (typeof ex1, ex1, ex2)
  | Syntax.ECall (e1, elist) ->
     let ex1 = ex e1 in
     (match typeof ex1 with
      | TPtr (TFun (retty, _)) ->
         Type.ECall (retty, ex1, List.map ex elist)
      | _ -> raise (TypingError "ECall: not a function given"))
  | Syntax.EAddr e ->
     let ex1 = ex e in
     Type.EAddr (TPtr (typeof ex1), ex1)
  | Syntax.EPtr e ->
     let ex1 = ex e in
     (match typeof ex1 with
      | TPtr ty -> Type.EPtr (ty, ex1)
      | _ -> raise (TypingError "ptr"))
  | Syntax.ECond (e1, e2, e3) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     let ex3 = ex e3 in
     if (typeof ex2) = (typeof ex3) then
       Type.ECond (typeof ex2, ex1, ex2, ex3)
     else
       raise (TypingError "cond")
  | Syntax.EDot (e1, Syntax.Name nm) ->
     let ex1 = ex e1 in
     let ty =  resolve_member_type (typeof ex1) nm in
     Type.EDot(ty, ex1, Type.Name nm)
  | Syntax.ECast (ty, e) ->
     let e = ex e in
     let ty2 = typeof e in
     Type.ECast (ty, ty2, e)
  | Syntax.ESizeof (ty) ->
     let i = sizeof ty in
     Type.EConst (TUnsigned, Type.VInt i)

let ex_opt = function
  | Some e ->
     Some (ex e)
  | None ->
     None

let dv = function
  | Syntax.Decl(ln, ty, Syntax.Name n, x) ->
     push_stack (n, ty) venv_ref;
     let init = initialize ty x in
     Type.Decl(ln, ty, Type.Name n, List.map ex init)

let rec st = function
  | Syntax.SNil -> Type.SNil
  | Syntax.SBlock(x, y) ->
     let x1 = List.map dv x in
     let y1 = List.map st y in
     let size = sum (List.map (fun (Syntax.Decl (_,ty,_,_)) -> sizeof ty * 4) x) in
     enter_block size;
     let s = Type.SBlock(x1, y1) in
     leave_block size;
     s
  | Syntax.SWhile (e, stmt) ->
     let e1 = ex e in
     let s1 = st stmt in
     Type.SWhile (e1, s1)
  | Syntax.SDoWhile (stmt, e) ->
     let s1 = st stmt in
     let e1 = ex e in
     Type.SDoWhile (s1, e1)
  | Syntax.SFor (e1, e2, e3, stmt) ->
     let oe1 = ex_opt e1 in
     let oe2 = ex_opt e2 in
     let oe3 = ex_opt e3 in
     let s1 = st stmt in
     Type.SFor (oe1, oe2, oe3, s1)
  | Syntax.SIfElse (e, s1, s2) ->
     let ex1 = ex e in
     let st1 = st s1 in
     let st2 = st s2 in
     Type.SIfElse (ex1, st1, st2)
  | Syntax.SReturn e ->
     let ex1 = ex_opt e in
     Type.SReturn (ex1)
  | Syntax.SContinue ->
     Type.SContinue
  | Syntax.SBreak ->
     Type.SBreak
  | Syntax.SLabel (str, stmt) ->
     let st1 = st stmt in
     Type.SLabel (str, st1)
  | Syntax.SGoto str ->
     Type.SGoto str
  | Syntax.SSwitch (e, stmt) ->
     let ex1 = ex e in
     let st1 = st stmt in
     Type.SSwitch (ex1, st1)
  | Syntax.SCase i ->
     Type.SCase i
  | Syntax.SDefault ->
     Type.SDefault
  | Syntax.SExpr e ->
     let ex1 = ex e in
     Type.SExpr ex1

let rec def = function
  | Syntax.DefFun (d, dlist, b) ->
     let d1 = dv d in
     let old_venv = !venv_ref in
     let old_senv = !senv_ref in
     let a1 = List.map dv dlist in
     sp_max := 0;
     let b1 = st b in
     let Type.Decl (_,_,Type.Name name,_) = d1 in
     append_info (name, !sp_max);
     let ret = Type.DefFun (d1, a1, b1) in
     venv_ref := old_venv;
     senv_ref := old_senv;
     ret
  | Syntax.DefVar decl ->
     Type.DefVar (dv decl)

let main defs =
  let go x =
    let Type.Decl (_, ty, Type.Name n, _) = dv x in (n, ty) in
  senv_ref := List.map
                (fun (mem,ds) -> (mem, List.map go ds))
                (List.rev !Syntax.struct_env);
  List.map (fun x -> def x) defs
