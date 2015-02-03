open Ctype
open Printf
open Util

let venv_ref : (string * ctype) list ref = ref []
let ret_ty_ref : ctype ref = ref TInt
let fun_name_ref : string ref = ref "global"

let raise_error fmt =
  ksprintf (fun s ->
    fprintf stderr "TypingError: %s (%s)\n" s !fun_name_ref;
    exit 1
  ) fmt

let get_ret_ty = function
  | Type.Decl (_, TFun (ty, _), _, _) -> ty
  | _ -> raise_error "get return type"

let resolve_var_type name =
  let rec go name  = function
    | [] -> raise_error "variable not found: %s" name
    | (s, ty)::_ when s=name -> ty
    | _ :: xs -> go name xs in
  go name !venv_ref

let resolve_member_type ty mem_name =
  match ty with
  | TStruct s_id ->
     let dvs = List.nth !struct_env s_id in
     List.assoc mem_name dvs
  | TUnion u_id ->
     let dvs = List.nth !union_env u_id in
     List.assoc mem_name dvs
  | _ -> failwith "resolve_member_type"

let typeof = function
  | Type.EArith  (t, _, _, _) -> t
  | Type.EFArith (t, _, _, _) -> t
  | Type.ERel    (t, _, _, _) -> t
  | Type.EURel   (t, _, _, _) -> t
  | Type.EFRel   (t, _, _, _) -> t
  | Type.EPAdd   (t, _, _) -> t
  | Type.EPDiff  (t, _, _) -> t
  | Type.EEq     (t, _, _, _) -> t
  | Type.EFEq    (t, _, _, _) -> t
  | Type.ELog    (t, _, _, _) -> t
  | Type.EUnary  (t, _, _) -> t
  | Type.EFUnary (t, _, _) -> t
  | Type.EPPost  (t, _, _) -> t
  | Type.EConst  (t, _) -> t
  | Type.EVar    (t, _) -> t
  | Type.EComma  (t, _, _) -> t
  | Type.EAssign (t, _, _, _) -> t
  | Type.EFAssign(t, _, _, _) -> t
  | Type.ECall   (t, _, _) -> t
  | Type.EAddr   (t, _) -> t
  | Type.EPtr    (t, _) -> t
  | Type.ECond   (t, _, _, _) -> t
  | Type.EDot    (t, _, _) -> t
  | Type.ECast   (t, _, _) -> t
  | Type.ENil -> failwith "typeof ENil"

let is_integral = function
  | TInt | TShort | TLong | TUInt | TChar -> true
  | _ -> false
let is_num = function
  | TInt | TShort | TLong | TUInt
  | TChar | TFloat -> true
  | _ -> false
let is_pointer = function
  | TPtr _ -> true
  | _ -> false
let is_num_or_ptr x = is_num x || is_pointer x

let arith_conv = function
  | t1, t2 when not (is_num t1 && is_num t2) -> None
  | TFloat, _ | _, TFloat -> Some TFloat
  | TLong,  _ | _, TLong  -> Some TLong
  | TUInt,  _ | _, TUInt  -> Some TUInt
  | _ -> Some TInt

let initialize ty init =
  let scaler = function
    | Syntax.IVect ((Syntax.IVect _)::_) ->
       raise_error "too many braces around scalar initializer"
    | Syntax.IVect [Syntax.IScal e] -> [e]
    | Syntax.IVect _ ->
       raise_error "invalid scaler initializer"
    | Syntax.IScal e -> [e] in
  let rec compound ty init idx =
    match ty, init with
    | TStruct s_id, Syntax.IVect ilist ->
       let s = List.nth !struct_env s_id in
       if List.length s = idx then ([], init)
       else
         let l, rem  = inner (snd (List.nth s idx)) ilist in
         let r, tail = compound ty rem (idx + 1) in
         (l @ r, tail)
    | TUnion u_id, Syntax.IVect ilist ->
       let u = List.nth !union_env u_id in
       inner (snd (List.hd u)) ilist
    | TArray (inner_ty, sz), Syntax.IVect ilist ->
       if sz > 0 && sz = idx then ([], init)
       else
         let l, rem = inner inner_ty ilist in
         if sz = 0 && rem = Syntax.IVect [] then (l, rem)
         else
           let r, tail = compound ty rem (idx + 1) in
           (l @ r, tail)
    | TArray (TChar, sz), Syntax.IScal (Syntax.EConst (Syntax.VStr str)) ->
       let str = if sz > 0 then List.rev (List.tl (List.rev str)) else str in
       let f i = Syntax.IScal (Syntax.EConst (Syntax.VInt i)) in
       let ilist = Syntax.IVect (List.map f str) in
       compound ty ilist 0
    | _ -> raise_error "requied initializer list"
  and inner inner_ty ilist =
    let i, is =
      if ilist = [] then
        Syntax.IScal (Syntax.EConst (Syntax.VInt 0)), []
      else
        List.hd ilist, List.tl ilist in
    match inner_ty, i with
    | TStruct _, Syntax.IVect _ | TArray _, Syntax.IVect _ ->
       let res, tail = compound inner_ty i 0 in
       if tail <> Syntax.IVect [] then
         raise_error "initializer eccess elements";
       (res, Syntax.IVect is)
    | TArray (TChar, sz), Syntax.IScal (Syntax.EConst (Syntax.VStr str)) ->
       let str = if sz > 0 then List.rev (List.tl (List.rev str)) else str in
       let f i = Syntax.IScal (Syntax.EConst (Syntax.VInt i)) in
       inner inner_ty (Syntax.IVect (List.map f str) :: is)
    | TStruct _, _ | TArray _, _ ->
       compound inner_ty (Syntax.IVect ilist) 0
    | _, _ -> (scaler i, Syntax.IVect is) in
  match init with
  | None -> []
  | Some init ->
     match ty with
     | TStruct _ | TUnion _ | TArray _ ->
        let res, tail = compound ty init 0 in
        if tail <> Syntax.IVect [] then
          raise_error "initializer eccess elements";
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
       | Syntax.VInt   i -> TInt,   Type.VInt i
       | Syntax.VFloat f -> TFloat, Type.VFloat f
       | Syntax.VStr   s -> TArray (TInt, List.length s), Type.VStr s in
     Type.EConst (ty, v)
  | Syntax.EVar name->
     if name = "__asm" then
       Type.EVar (TFun (TVoid, [TPtr TChar]), name)
     else
       Type.EVar (resolve_var_type name, name)
  | Syntax.EComma (e1, e2) ->
     let e = ex e2 in
     Type.EComma(typeof e, ex e1, e)
  | Syntax.EArith (op, e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     let ty1 = typeof ex1 in
     let ty2 = typeof ex2 in
     begin match op with
     | Add ->
        begin match (ty1, ty2) with
        | (TPtr ty, i) when is_integral i ->
           Type.EPAdd (TPtr ty, ex1, ex2)
        | (i, TPtr ty) when is_integral i ->
           Type.EPAdd (TPtr ty, ex2, ex1)
        | _ ->
           begin match arith_conv (ty1, ty2) with
           | Some TFloat ->
              Type.EFArith (TFloat, Add,
                            Type.ECast(TFloat, ty1, ex1),
                            Type.ECast(TFloat, ty2, ex2))
           | Some ty ->
              Type.EArith (ty, Add, ex1, ex2)
           | None ->
              raise_error "EArith: add"
           end
        end
     | Sub ->
        begin match (ty1, ty2) with
        | (TPtr ty1, TPtr ty2) when ty1 = ty2 ->
           Type.EPDiff(TInt, ex1, ex2)
        | (TPtr ty1, i) when is_integral i ->
           let m_ex2 = ex (Syntax.EUnary(Minus, e2)) in
           assert (is_integral (typeof m_ex2));
           Type.EPAdd (TPtr ty1, ex1, m_ex2)
        | _ ->
           begin match arith_conv (ty1, ty2) with
           | Some TFloat ->
              Type.EFArith (TFloat, Sub,
                            Type.ECast(TFloat, ty1, ex1),
                            Type.ECast(TFloat, ty2, ex2))
           | Some ty ->
              Type.EArith (ty, Sub, ex1, ex2)
           | None ->
              raise_error "EArith: add"
           end
        end
     | _ ->
        begin match arith_conv (ty1, ty2) with
        | Some TFloat ->
           begin match op with
           | Mul | Div ->
              Type.EFArith (TFloat, op,
                            Type.ECast(TFloat, ty1, ex1),
                            Type.ECast(TFloat, ty2, ex2))
           | _ ->
              raise_error "EFArith; float"
           end
        | Some ty ->
           Type.EArith (ty, op, ex1, ex2)
        | None -> raise_error "EArith"
        end
     end
  | Syntax.ERel (op, e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     begin match (typeof ex1, typeof ex2) with
     | (TPtr _, TPtr _) ->
        Type.EURel (TInt, op, ex1, ex2)
     | (ty1, ty2) ->
        begin match arith_conv (ty1, ty2) with
        | Some TFloat ->
           Type.EFRel (TInt, op,
                       Type.ECast(TFloat, ty1, ex1),
                       Type.ECast(TFloat, ty2, ex2))
        | Some TUInt ->
           Type.EURel (TInt, op, ex1, ex2)
        | Some _ ->
           Type.ERel (TInt, op, ex1, ex2)
        | None ->
           raise_error "relation"
        end
     end
  | Syntax.EEq (op, e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     begin match (typeof ex1, typeof ex2) with
     | (TPtr _, TPtr _) ->
        Type.EEq (TInt, op, ex1, ex2)
     | (t, TPtr _) when is_integral t ->
        begin match ex1 with
        | Type.EConst (_, Type.VInt 0) -> (* null pointer *)
           Type.EEq (TInt, op, ex1, ex2)
        | _ ->
           raise_error "eq: pointer and non-zero integer"
        end
     | (TPtr _, t) when is_integral t ->
        ex (Syntax.EEq (op, e2, e1))
     | (t1, t2) ->
        begin match arith_conv (t1, t2) with
        | Some TFloat ->
           Type.EFEq (TInt, op,
                      Type.ECast(TFloat, t1, ex1),
                      Type.ECast(TFloat, t2, ex2))
        | Some _ ->
           Type.EEq (TInt, op, ex1, ex2)
        | _ ->
           raise_error "eq: otherwise"
        end
     end
  | Syntax.ELog (op, e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     let t1 = typeof ex1 in
     let t2 = typeof ex2 in
     if (is_num_or_ptr t1) || (is_num_or_ptr t2) then
       Type.ELog (TInt, op, ex1, ex2)
     else
       raise_error "logical"
  | Syntax.EUnary (op, e1) ->
     let ex1 = ex e1 in
     begin match (op, typeof ex1) with
     | (PostInc, TPtr t) ->
        Type.EPPost(TPtr t, Inc, ex1)
     | (PostDec, TPtr t) ->
        Type.EPPost(TPtr t, Dec, ex1)
     | (Plus,  TFloat)
     | (Minus, TFloat) ->
        Type.EFUnary(TFloat, op, ex1)
     | (LogNot, _) (* ! *)
     | (_, TInt)
     | (_, TShort)
     | (_, TChar) ->
        Type.EUnary(TInt, op, ex1)
     | (_, TLong) ->
        Type.EUnary(TLong, op, ex1)
     | (_, TUInt) ->
        Type.EUnary(TUInt, op, ex1)
     | _ ->
        raise_error "unary"
     end
  | Syntax.EAssign (op, e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     begin match (typeof ex1, typeof ex2) with
     | (ty1, ty2) when is_integral ty1 && ty1 = ty2 ->
        Type.EAssign (ty1, op, ex1, ex2)
     | (ty1, ty2) when is_integral ty1 && is_num ty2 ->
        Type.EAssign (ty1, op, ex1,
                      Type.ECast(ty1, ty2, ex2))
     | (ty1, ty2) when ty1 = TFloat && is_num ty2 ->
        Type.EFAssign (TFloat, op, ex1,
                       Type.ECast(ty1, ty2, ex2))
     | (TPtr ty, i) when is_integral i ->
        begin match op with
        | None ->
           Type.EAssign (TPtr ty, op, ex1, ex2)
        | Some Add ->
           Type.EAssign (TPtr ty, op, ex1, ex2)
        | Some Sub ->
           let m_ex2 = ex (Syntax.EUnary(Minus, e2)) in
           Type.EAssign (TPtr ty, Some Add, ex1, m_ex2)
        | _ ->
           raise_error "EAssign: TPtr"
        end
     | (ty1, ty2) ->
        if op = None then
          Type.EAssign (ty1, op, ex1,
                        Type.ECast(ty1, ty2, ex2))
        else
          raise_error "EAssign"
     end
  | Syntax.ECall (e1, elist) ->
     let ex1 = ex e1 in
     begin match typeof ex1 with
     | TPtr (TFun (retty, argtys)) ->
        let go a t =
          let arg = ex a in
          if typeof arg = t then
            arg
          else
            Type.ECast(t, typeof arg, arg) in
        let args =
          if List.length elist = List.length argtys then
            List.map2 go elist argtys
          else (* for variadic function *)
            List.map ex elist in
        Type.ECall (retty, ex1, args)
     | _ ->
        raise_error "ECall: not a function given"
     end
  | Syntax.EAddr e ->
     let ex1 = ex e in
     Type.EAddr (TPtr (typeof ex1), ex1)
  | Syntax.EPtr e ->
     let ex1 = ex e in
     begin match typeof ex1 with
     | TPtr ty -> Type.EPtr (ty, ex1)
     | _ -> raise_error "ptr"
     end
  | Syntax.ECond (e1, e2, e3) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     let ex3 = ex e3 in
     let ty2 = typeof ex2 in
     let ty3 = typeof ex3 in
     if ty2 = ty3 then
       Type.ECond (ty2, ex1, ex2, ex3)
     else
       begin match arith_conv (ty2, ty3) with
       | Some ty ->
          Type.ECond (ty, ex1,
                      Type.ECast (ty, ty2, ex2),
                      Type.ECast (ty, ty3, ex3))
       | None ->
          raise_error "cond"
       end
  | Syntax.EDot (e1, name) ->
     let ex1 = ex e1 in
     let ty = resolve_member_type (typeof ex1) name in
     Type.EDot(ty, ex1, name)
  | Syntax.ECast (ty, e) ->
     let e = ex e in
     let ty2 = typeof e in
     Type.ECast (ty, ty2, e)
  | Syntax.ESizeof (ty) ->
     let i = sizeof ty in
     Type.EConst (TUInt, Type.VInt i)
  | Syntax.ESizeofExpr (e) ->
     let i = sizeof (typeof (ex' e)) in
     Type.EConst (TUInt, Type.VInt i)

let ex_opt = function
  | Some e ->
     Some (ex e)
  | None ->
     None

let dv = function
  | Syntax.Decl(ln, ty, name, x) ->
     let init = initialize ty x in
     let ty =
       match ty with
       | TArray (t, 0) ->
          TArray (t, List.length init / sizeof t)
       | _ -> ty in
     push venv_ref (name, ty);
     Type.Decl(ln, ty, name, List.map ex init)

let rec st = function
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
     begin match ex_opt e with
     | None ->
        Type.SReturn (None)
     | Some ex1 ->
        let ty = typeof ex1 in
        if ty = !ret_ty_ref then
          Type.SReturn (Some ex1)
        else
          Type.SReturn (Some (Type.ECast(!ret_ty_ref, ty, ex1)))
     end
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

let def = function
  | Syntax.DefFun (d, dlist, b) ->
     let Syntax.Decl( _, _, fname, _) = d in
     fun_name_ref := fname;
     let d1 = dv d in
     ret_ty_ref := get_ret_ty d1;
     let old_venv = !venv_ref in
     let a1 = List.map dv dlist in
     let b1 = st b in
     let ret = Type.DefFun (d1, a1, b1) in
     venv_ref := old_venv;
     fun_name_ref := "";
     ret
  | Syntax.DefVar decl ->
     Type.DefVar (dv decl)

let main defs =
  List.map def defs
