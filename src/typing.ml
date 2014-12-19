open Ctype
open Format
exception TypingError of string
exception TODO of string
exception Unreachable
type name = string
let venv_ref : (string * ctype) list ref = ref [];;
let fenv_ref : (string * ctype) list ref = ref [];;

(* This is initialized in main *)
let senv_ref : (int * ((name * ctype) list)) list ref = ref [];;

let push_stack x env =
  env := x::!env
let resolve_var_type nm =
  let rec go nm  = function
    | [] -> TInt
    | (s, ty)::_ when s=nm -> ty
    | _ :: xs -> go nm xs in
  go nm !venv_ref
let resolve_fun_type nm =
  let rec go nm  = function
    | [] -> TInt
    | (s, ty)::_ when s=nm -> ty
    | _ :: xs -> go nm xs in
  go nm !fenv_ref
let resolve_member_type stct mem_name =
  match stct with
  | TStruct s_id ->
     let dvs = List.assoc s_id !senv_ref in
     List.assoc mem_name dvs
  | _ -> raise Unreachable

let rec main defs =
  let go x =
    match dv x with
    | Type.Decl (ty, Type.Name n, _) -> (n, ty) in
  senv_ref := List.map
                (fun (mem,ds) -> (mem, List.map go ds))
                (List.rev !Syntax.struct_env);
  List.map (fun x -> def x) defs
and def = function
  | Syntax.DefFun (Syntax.Decl (TFun(ty, _), Syntax.Name n, None), dlist, b) ->
     push_stack (n, ty) fenv_ref;
     let old_venv = !venv_ref in
     let old_fenv = !fenv_ref in
     let old_senv = !senv_ref in
     let a1 = List.map dv dlist in
     let b1 = st b in
     let ret = Type.DefFun (ty, Type.Name n, a1, b1) in
     venv_ref := old_venv;
     fenv_ref := old_fenv;
     senv_ref := old_senv;
     ret
  | Syntax.DefVar decl ->
     Type.DefVar (dv decl)
  | _ -> raise (TypingError "def")
and dv = function
  | Syntax.Decl(ty, Syntax.Name n, x) ->
     push_stack (n, ty) venv_ref;
     let init = initialize ty x in
     Type.Decl(ty, Type.Name n, List.map ex init)
and initialize ty init =
  let rec go_compound ty init idx =
    match init with
    | Syntax.IList ilist ->
      begin match ty with
      | TStruct s_id ->
        let s = List.assoc s_id !senv_ref in
        if List.length s = idx then [], init
        else
          let l, rem  = go_inner (snd (List.nth s idx)) ty ilist in
          let r, tail = go_compound ty rem (idx + 1) in
          l @ r, tail
      | TArray (inner_ty, sz) ->
        if sz = idx then [], init
        else
          let l, rem  = go_inner inner_ty ty ilist in
          let r, tail = go_compound ty rem (idx + 1) in
          l @ r, tail
      | _ -> raise (TypingError "initialize: internal error")
      end
    | Syntax.IScal (Syntax.EConst (Syntax.VStr str)) ->
       let ilist = Syntax.IList (List.map (fun i -> Syntax.IScal (Syntax.EConst (Syntax.VInt i))) str) in
       go_compound ty ilist 0
    | _ -> raise (TypingError "requied initializer list")
  and go_inner inner_ty ty ilist =
    let i, is =
      if ilist = [] then
        Syntax.IScal (Syntax.EConst (Syntax.VInt 0)), []
      else
        List.hd ilist, List.tl ilist in
    match inner_ty, i with
    | TStruct _, Syntax.IList _ | TArray _, Syntax.IList _ ->
      let res, tail = go_compound inner_ty i 0 in
      if tail <> Syntax.IList [] then
        raise (TypingError "initializer eccess elements");
      res, Syntax.IList is
    | TStruct _, _ | TArray _, _ ->
      go_compound inner_ty (Syntax.IList ilist) 0
    | _, _ -> scaler i, Syntax.IList is
  and scaler = function
    | Syntax.IList ((Syntax.IList _)::_) ->
      raise (TypingError "too many braces around scalar initializer")
    | Syntax.IList [Syntax.IScal e] -> [e]
    | Syntax.IList _ ->
      raise (TypingError "invalid scaler initializer")
    | Syntax.IScal e -> [e] in
  match init with
  | None -> []
  | Some init ->
    match ty with
    | TStruct _ | TArray _ ->
      let res, tail = go_compound ty init 0 in
      if tail <> Syntax.IList [] then
        raise (TypingError "initializer eccess elements");
      res
    | _ -> scaler init
and st = function
  | Syntax.SNil -> Type.SNil
  | Syntax.SBlock(x, y) ->
     let x1 = List.map dv x in
     let y1 = List.map st y in
     Type.SBlock(x1, y1)
  | Syntax.SWhile (e, stmt) ->
     let e1 = ex e in
     let s1 = st stmt in
     Type.SWhile (e1, s1)
  | Syntax.SDoWhile (stmt, e) ->
     let s1 = st stmt in
     let e1 = ex e in
     Type.SDoWhile (s1, e1)
  | Syntax.SFor (e1, e2, e3, stmt) ->
     let oe1 = opex e1 in
     let oe2 = opex e2 in
     let oe3 = opex e3 in
     let s1 = st stmt in
     Type.SFor (oe1, oe2, oe3, s1)
  | Syntax.SIfElse (e, s1, s2) ->
     let ex1 = ex e in
     let st1 = st s1 in
     let st2 = st s2 in
     Type.SIfElse (ex1, st1, st2)
  | Syntax.SReturn e ->
     let ex1 = ex e in
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
and opex = function
  | Some x ->
     let ex1 = ex x in
     Some ex1
  | None -> None
and ex e =
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
      | Syntax.Add ->
         (match (typeof ex1, typeof ex2) with
          | (TPtr ty, i) when is_integral i ->
             Type.EPAdd (TPtr ty, ex1, ex2)
          | (i, TPtr ty) when is_integral i ->
             Type.EPAdd (TPtr ty, ex2, ex1)
          | (ty1, ty2) when is_integral ty1 && is_integral ty2 ->
             let ty = int_conv (ty1,ty2) in
             Type.EArith (ty, Type.Add, ex1, ex2)
          | _ -> raise (TypingError "EArith: add"))
      | Syntax.Sub ->
         (match (typeof ex1, typeof ex2) with
          | (TPtr ty1, TPtr ty2) ->
             Type.EPDiff(TInt, ex1, ex2)
          | (TPtr ty1, i) when is_integral i ->
             let m_ex2 = ex (Syntax.EUnary(Syntax.Minus, e2)) in
             assert (is_integral (typeof m_ex2));
             Type.EPAdd (TPtr ty1, ex1, m_ex2)
          | (ty1, ty2) when is_integral ty1 && is_integral ty2 ->
             let ty = int_conv (ty1,ty2) in
             Type.EArith (ty, Type.Sub, ex1, ex2)
          | _ -> raise (TypingError "EArith: sub"))
      | _ ->
         (match (typeof ex1, typeof ex2) with
          | (t1, t2) when is_integral t1 && is_integral t2->
             let ty = int_conv (t1, t2) in
             let op = arith_bin_op op in
             Type.EArith (ty, op, ex1, ex2)
          | _ -> raise (TypingError "EArith")))
  | Syntax.ERel (rop, e1, e2) ->
     let op  = rel_bin_op rop in
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     (match (typeof ex1, typeof ex2) with
      | (t1, t2) when is_integral t1 && is_integral t2 ->
         if int_conv (t1, t2) = TUnsigned then
           raise (TypingError "relation: unsigned")
         else
           Type.ERel (TInt, op, ex1, ex2)
      | (TPtr _, TPtr _) ->
         raise (TypingError "relation: pointer")
      | _ ->
         raise (TypingError "relation"))
  | Syntax.EEq (eop, e1, e2) ->
     let op  = eq_bin_op eop in
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     (match (typeof ex1, typeof ex2) with
      | (t1, t2) when is_integral t1 && is_integral t2 ->
         if int_conv (t1, t2) = TUnsigned then
           raise (TypingError "eq: unsigned")
         else
           Type.EEq (TInt, op, ex1, ex2)
      | (TPtr _, TPtr _) ->
         raise (TypingError "eq: pointer")
      | _ ->
         raise (TypingError "eq: otherwise"))
  | Syntax.ELog (lop, e1, e2) ->
     let op  = logical_bin_op lop in
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     (match (typeof ex1, typeof ex2) with
      | (t1, t2) when is_integral t1 && is_integral t2 ->
         if int_conv (t1, t2) = TUnsigned then
           raise (TypingError "logical: unsigned")
         else
           Type.ELog (TInt, op, ex1, ex2)
      | _ ->
         raise (TypingError "logical"))
  | Syntax.EUnary (op1, e1) ->
     let op = unary_op op1 in
     let ex1= ex e1 in
     (match (op, typeof ex1) with
      | (Type.LogNot, _)
      | (_, TInt)
      | (_, TChar) ->
         Type.EUnary(TInt, op, ex1)
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
     Type.ECall (typeof ex1, ex1, List.map ex elist)
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
     let i = size_of ty in
     Type.EConst (TUnsigned, Type.VInt i)
and arith_bin_op = function
  | Syntax.Add -> Type.Add
  | Syntax.Sub -> Type.Sub
  | Syntax.Mul -> Type.Mul
  | Syntax.Div -> Type.Div
  | Syntax.Mod -> Type.Mod
  | Syntax.LShift -> Type.LShift
  | Syntax.RShift -> Type.RShift
  | Syntax.BitAnd -> Type.BitAnd
  | Syntax.BitXor -> Type.BitXor
  | Syntax.BitOr -> Type.BitOr
and rel_bin_op = function
  | Syntax.Lt -> Type.Lt
  | Syntax.Le -> Type.Le
  | Syntax.Gt -> Type.Gt
  | Syntax.Ge -> Type.Ge
and eq_bin_op = function
  | Syntax.Eq -> Type.Eq
  | Syntax.Ne -> Type.Ne
and logical_bin_op = function
  | Syntax.And -> Type.And
  | Syntax.Or -> Type.Or
and unary_op = function
  | Syntax.Plus -> Type.Plus
  | Syntax.Minus -> Type.Minus
  | Syntax.BitNot -> Type.BitNot
  | Syntax.LogNot -> Type.LogNot
  | Syntax.PostInc -> Type.PostInc
  | Syntax.PostDec -> Type.PostDec
and typeof = function
  | Type.EArith  (t, _, _, _) -> t
  | Type.ERel    (t, _, _, _) -> t
  | Type.EPAdd   (t, _, _) -> t
  | Type.EPDiff  (t, _, _) -> t
  | Type.EEq     (t, _, _, _) -> t
  | Type.ELog    (t, _, _, _) -> t
  | Type.EUnary  (t, _, _) -> t
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
and is_integral = function
  | TInt
  | TUnsigned
  | TChar -> true
  | _ -> false
and int_conv = function
  | (TUnsigned, _)
  | (_, TUnsigned) ->
     TUnsigned
  | _ ->
     TInt
and size_of = function
  | TInt
  | TUnsigned
  | TChar
  | TPtr _ -> 1
  | TArray (ty, sz) -> sz * (size_of ty)
  | TFun _ -> raise (TypingError "sizeof function")
  | TStruct sid ->
     let sz fs = List.fold_left (fun num (_, ty) -> num + (size_of ty)) 0 fs in
     let rec go = function
       | [] -> raise (TypingError (sprintf "struct %d not found" sid))
       | (s, ms)::_ when s = sid -> sz ms
       | _::zs -> go zs in
     go !senv_ref
