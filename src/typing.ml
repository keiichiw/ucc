open Ctype
open Printf
open Util
open Type

let venv_ref : (string * ctype) list ref = ref []
let ret_ty_ref : ctype ref = ref TInt
let fun_name_ref : string ref = ref "global"

let raise_error fmt =
  ksprintf (fun s ->
    fprintf stderr "TypingError: %s (%s)\n" s !fun_name_ref;
    exit 1
  ) fmt

let get_ret_ty = function
  | Decl (_, TFun (ty, _), _, _) -> ty
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

let is_null = function
  | EConst (_, VInt 0) -> true
  | _ -> false

let arith_conv = function
  | t1, t2 when not (is_arith t1 && is_arith t2) -> None
  | TDouble, _ | _, TDouble -> Some TDouble
  | TFloat,  _ | _, TFloat  -> Some TFloat
  | TULong,  _ | _, TULong  -> Some TULong
  | TLong, TUInt | TUInt, TLong  -> Some TULong
  | TLong,  _ | _, TLong  -> Some TLong
  | TUInt,  _ | _, TUInt  -> Some TUInt
  | _ -> Some TInt

let initialize ty init =
  let scalar ty = function
    | Syntax.IVect ((Syntax.IVect _)::_) ->
       raise_error "too many braces around scalar initializer"
    | Syntax.IVect [Syntax.IScal e] -> [(ty, e)]
    | Syntax.IVect _ ->
       raise_error "invalid scalar initializer"
    | Syntax.IScal e -> [(ty, e)] in
  let rec compound ty init idx =
    let pad = (TVoid, Syntax.EPadding) in
    match ty, init with
    | TStruct s_id, Syntax.IVect ilist ->
       let s = List.nth !struct_env s_id in
       if List.length s = idx then ([pad], init)
       else
         let l, rem  = inner (snd (List.nth s idx)) ilist in
         let r, tail = compound ty rem (idx + 1) in
         (l @ r, tail)
    | TUnion u_id, Syntax.IVect ilist ->
       let u = List.nth !union_env u_id in
       let (l, r) = inner (snd (List.hd u)) ilist in
       (l @ [pad], r)
    | TArray (inner_ty, sz), Syntax.IVect ilist ->
       if sz > 0 && sz = idx then ([pad], init)
       else
         let l, rem = inner inner_ty ilist in
         if sz = 0 && rem = Syntax.IVect [] then (l @ [pad], rem)
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
    | _, _ -> (scalar inner_ty i, Syntax.IVect is) in
  match init with
  | None -> []
  | Some init ->
     match ty with
     | TStruct _ | TUnion _ | TArray _ ->
        let res, tail = compound ty init 0 in
        if tail <> Syntax.IVect [] then
          raise_error "initializer eccess elements";
        res
     | _ -> scalar ty init

let rec deref_cast = function
  | ECast (t1, t2, e) when t1 = t2 -> e
  | ECast (ty, _, e) as expr ->
     begin match deref_cast e with
     | EConst (_, VInt i) when is_real ty ->
        EConst (ty, VFloat (float i))
     | EConst (_, VFloat f) when is_integral ty ->
        EConst (ty, VInt (truncate f))
     | EConst (_, v) ->
        EConst (ty, v)
     | _ ->
        expr
     end
  | e -> e

let fold_int_expr f ty e1 e2 expr =
  match deref_cast e1, deref_cast e2 with
  | EConst (_, VInt i1), EConst (_, VInt i2) ->
     EConst (ty, VInt (f i1 i2))
  | _ -> expr

let fold_float_expr f ty e1 e2 expr =
  match deref_cast e1, deref_cast e2 with
  | EConst (_, VFloat f1), EConst (_, VFloat f2) ->
     EConst (ty, VFloat (f f1 f2))
  | _ -> expr

let fold_ftoi_expr f e1 e2 expr =
  match deref_cast e1, deref_cast e2 with
  | EConst (_, VFloat f1), EConst (_, VFloat f2) ->
     EConst (TInt, VInt (f f1 f2))
  | _ -> expr

let fold_expr = function
  | EConst _ as e -> e
  | EArith (ty, op, e1, e2) as expr ->
     fold_int_expr (arith2fun ty op) ty e1 e2 expr
  | EFArith (ty, op, e1, e2) as expr ->
     fold_float_expr (farith2fun op) ty e1 e2 expr
  | EPAdd (ty, e1, e2) as expr ->
     let sz = sizeof (deref_pointer ty) in
     fold_int_expr (fun x y -> x + sz * y) ty e1 e2 expr
  | EPDiff (_, e1, e2) as expr ->
     let sz = sizeof (deref_pointer (typeof e1)) in
     fold_int_expr (fun x y -> (x - y) / sz) TInt e1 e2 expr
  | ERel (_, op, e1, e2) as expr ->
     fold_int_expr (rel2fun op) TInt e1 e2 expr
  | EURel (_, op, e1, e2) as expr ->
     fold_int_expr (urel2fun op) TInt e1 e2 expr
  | EFRel (_, op, e1, e2) as expr ->
     fold_ftoi_expr (rel2fun op) e1 e2 expr
  | EEq (_, op, e1, e2) as expr ->
     fold_int_expr (eq2fun op) TInt e1 e2 expr
  | EFEq (_, op, e1, e2) as expr ->
     fold_ftoi_expr (eq2fun op) e1 e2 expr
  | ELog (_, LogAnd, e1, e2) as expr ->
     begin match deref_cast e1, deref_cast e2 with
     | EConst (_, VInt 0), _
     | EConst (_, VInt _), EConst (_, VInt 0) ->
        EConst (TInt, VInt 0)
     | EConst (_, VInt _), EConst (_, VInt _) ->
        EConst (TInt, VInt 1)
     | _ -> expr
     end
  | ELog (_, LogOr, e1, e2) as expr ->
     begin match deref_cast e1, deref_cast e2 with
     | EConst (_, VInt x), _ when x <> 0 ->
        EConst (TInt, VInt 1)
     | EConst (_, VInt 0), EConst (_, VInt x) when x <> 0 ->
        EConst (TInt, VInt 1)
     | EConst (_, VInt 0), EConst (_, VInt 0) ->
        EConst (TInt, VInt 0)
     | _ -> expr
     end
  | EUnary (ty, op, e) as expr ->
     if op = PostInc || op = PostDec then expr else
     begin match deref_cast e with
     | EConst (_, VInt i) ->
        EConst (ty, VInt ((unary2fun op) i))
     | _ -> expr
     end
  | EFUnary (ty, op, e) as expr ->
     assert (op = Plus || op = Minus);
     begin match deref_cast e with
     | EConst (ty, VFloat f) ->
        EConst (ty, VFloat (if op = Plus then f else -.f))
     | _ -> expr
     end
  | ECond (ty, e1, e2, e3) as expr ->
     begin match deref_cast e1 with
     | EConst (_, VInt 0) -> deref_cast e3
     | EConst (_, VInt _) -> deref_cast e2
     | _ -> expr
     end
  | expr -> deref_cast expr

let rec ex e =
  let e = ex' e in
  if e = EPadding then e else
  match typeof e with
  | TArray (ty, _) ->
     EAddr (TPtr ty, e)
  | TFun _ as ty ->
     EAddr (TPtr ty, e)
  | _ ->
     fold_expr e

and ex' = function
  | Syntax.EConst v ->
     let (ty, v) = match v with
       | Syntax.VInt   i -> TInt,   VInt i
       | Syntax.VFloat f -> TFloat, VFloat f
       | Syntax.VStr   s -> TArray (TChar, List.length s), VStr s in
     EConst (ty, v)
  | Syntax.EVar name ->
     if name = "__asm" then
       EVar (TFun (TVoid, [TPtr TChar]), name)
     else
       EVar (resolve_var_type name, name)
  | Syntax.EComma (e1, e2) ->
     let e = ex e2 in
     EComma(typeof e, ex e1, e)
  | Syntax.EArith (op, e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     let ty1 = typeof ex1 in
     let ty2 = typeof ex2 in
     begin match op with
     | Add ->
        begin match (ty1, ty2) with
        | (TPtr ty, i) when is_integral i ->
           EPAdd (TPtr ty, ex1, ex2)
        | (i, TPtr ty) when is_integral i ->
           EPAdd (TPtr ty, ex2, ex1)
        | _ ->
           begin match arith_conv (ty1, ty2) with
           | Some ty when is_real ty ->
              EFArith (ty, Add,
                       ECast(ty, ty1, ex1),
                       ECast(ty, ty2, ex2))
           | Some ty ->
              EArith (ty, Add, ex1, ex2)
           | None ->
              raise_error "EArith: add"
           end
        end
     | Sub ->
        begin match (ty1, ty2) with
        | (TPtr ty1, TPtr ty2) when ty1 = ty2 ->
           EPDiff(TInt, ex1, ex2)
        | (TPtr ty1, i) when is_integral i ->
           let m_ex2 = ex (Syntax.EUnary(Minus, e2)) in
           assert (is_integral (typeof m_ex2));
           EPAdd (TPtr ty1, ex1, m_ex2)
        | _ ->
           begin match arith_conv (ty1, ty2) with
           | Some ty when is_real ty ->
              EFArith (ty, Sub,
                       ECast(ty, ty1, ex1),
                       ECast(ty, ty2, ex2))
           | Some ty ->
              EArith (ty, Sub, ex1, ex2)
           | None ->
              raise_error "EArith: sub"
           end
        end
     | LShift | RShift ->
        begin match arith_conv (ty1, TInt) with
        | Some ty when is_integral ty2 ->
           EArith (ty, op, ex1, ex2)
        | _ ->
           raise_error "EArith: shl/shr"
        end
     | _ ->
        begin match arith_conv (ty1, ty2) with
        | Some ty when is_real ty ->
           begin match op with
           | Mul | Div ->
              EFArith (ty, op,
                       ECast(ty, ty1, ex1),
                       ECast(ty, ty2, ex2))
           | _ ->
              raise_error "EFArith; float"
           end
        | Some ty ->
           EArith (ty, op, ex1, ex2)
        | None -> raise_error "EArith"
        end
     end
  | Syntax.ERel (op, e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     begin match (typeof ex1, typeof ex2) with
     | (TPtr _, TPtr _) ->
        EURel (TInt, op, ex1, ex2)
     | (ty1, ty2) ->
        begin match arith_conv (ty1, ty2) with
        | Some ty when is_real ty ->
           EFRel (TInt, op,
                  ECast(ty, ty1, ex1),
                  ECast(ty, ty2, ex2))
        | Some TUInt
        | Some TULong ->
           EURel (TInt, op, ex1, ex2)
        | Some _ ->
           ERel (TInt, op, ex1, ex2)
        | None ->
           raise_error "relation"
        end
     end
  | Syntax.EEq (op, e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     begin match (typeof ex1, typeof ex2) with
     | (TPtr _, TPtr _) ->
        EEq (TInt, op, ex1, ex2)
     | (_, TPtr _) when is_null ex1 ->
        EEq (TInt, op, ex1, ex2)
     | (TPtr _, _) when is_null ex2 ->
        EEq (TInt, op, ex2, ex1)
     | (t1, t2) ->
        begin match arith_conv (t1, t2) with
        | Some ty when is_real ty ->
           EFEq (TInt, op,
                 ECast(ty, t1, ex1),
                 ECast(ty, t2, ex2))
        | Some _ ->
           EEq (TInt, op, ex1, ex2)
        | _ ->
           raise_error "eq"
        end
     end
  | Syntax.ELog (op, e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     let t1 = typeof ex1 in
     let t2 = typeof ex2 in
     if is_scalar t1 || is_scalar t2 then
       ELog (TInt, op, ex1, ex2)
     else
       raise_error "logical"
  | Syntax.EUnary (op, e1) ->
     let ex1 = ex e1 in
     begin match (op, typeof ex1) with
     | (PostInc, TPtr t) ->
        EPPost(TPtr t, Inc, ex1)
     | (PostDec, TPtr t) ->
        EPPost(TPtr t, Dec, ex1)
     | (Plus,  ty)
     | (Minus, ty) when is_real ty ->
        EFUnary(ty, op, ex1)
     | (LogNot, ty) when is_real ty ->
        EFEq (TInt, Ne, ex1, EConst (ty, VFloat 0.0))
     | (LogNot, _) ->
        EUnary(TInt, op, ex1)
     | (_, t) ->
        begin match arith_conv (t, TInt) with
        | Some t ->
           EUnary(t, op, ex1)
        | None ->
           raise_error "unary"
        end
     end
  | Syntax.EAssign (op, e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     begin match (typeof ex1, typeof ex2) with
     | (ty1, ty2) when is_integral ty1 && ty1 = ty2 ->
        EAssign (ty1, op, ex1, ex2)
     | (ty1, ty2) when is_integral ty1 && is_arith ty2 ->
        EAssign (ty1, op, ex1, ECast(ty1, ty2, ex2))
     | (ty1, ty2) when is_real ty1 && is_arith ty2 ->
        EFAssign (ty1, op, ex1, ECast(ty1, ty2, ex2))
     | (TPtr ty, i) when is_integral i ->
        begin match op with
        | None ->
           EAssign (TPtr ty, op, ex1, ex2)
        | Some Add ->
           EAssign (TPtr ty, op, ex1, ex2)
        | Some Sub ->
           let m_ex2 = ex (Syntax.EUnary(Minus, e2)) in
           EAssign (TPtr ty, Some Add, ex1, m_ex2)
        | _ ->
           raise_error "EAssign: TPtr"
        end
     | (ty1, ty2) ->
        if op = None then
          if ty1 = ty2 then
            EAssign (ty1, op, ex1, ex2)
          else
            EAssign (ty1, op, ex1, ECast(ty1, ty2, ex2))
        else
          raise_error "EAssign"
     end
  | Syntax.ECall (Syntax.EVar ("__asm"),
                 [Syntax.EConst(Syntax.VStr asm)]) ->
    EAsm (TVoid, asm)
  | Syntax.ECall (e1, elist) ->
     let ex1 = ex e1 in
     begin match typeof ex1 with
     | TPtr (TFun (retty, argtys)) ->
        let go a t =
          let arg = ex a in
          if t = TVoid || typeof arg = t then
            arg
          else
            ECast(t, typeof arg, arg) in
        let exlen = List.length elist in
        let tylen = List.length argtys in
        let args =
          if exlen = tylen then
            List.map2 go elist argtys
          else if exlen > tylen then (* for variadic function *)
            let argtys = argtys @ (rep TVoid (exlen - tylen)) in
            List.map2 go elist argtys
          else
            List.map ex elist in
        if retty = TVoid || sizeof retty <= 4 then
          ECall (retty, ex1, args)
        else
          raise_error "ECall : function that returns struct is not supported"
     | _ ->
        raise_error "ECall: not a function given"
     end
  | Syntax.EAddr e ->
     let ex1 = ex e in
     EAddr (TPtr (typeof ex1), ex1)
  | Syntax.EPtr e ->
     let ex1 = ex e in
     begin match typeof ex1 with
     | TPtr ty -> EPtr (ty, ex1)
     | _ -> raise_error "ptr"
     end
  | Syntax.ECond (e1, e2, e3) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     let ex3 = ex e3 in
     let ty1 = typeof ex1 in
     let ty2 = typeof ex2 in
     let ty3 = typeof ex3 in
     let ex1 =
       if is_real ty1 then
         fold_expr (EFEq (TInt, Ne, ex1, EConst (ty1, VFloat 0.0)))
       else ex1 in
     if ty2 = ty3 then
       ECond (ty2, ex1, ex2, ex3)
     else if is_pointer ty2 && (ty3 = TPtr TVoid || is_null ex3) then
       ECond (ty2, ex1, ex2, ECast (ty2, ty3, ex3))
     else if is_pointer ty3 && (ty2 = TPtr TVoid || is_null ex2) then
       ECond (ty3, ex1, ECast (ty3, ty2, ex2), ex3)
     else
       begin match arith_conv (ty2, ty3) with
       | Some ty ->
          ECond (ty, ex1,
                 ECast (ty, ty2, ex2),
                 ECast (ty, ty3, ex3))
       | None ->
          raise_error "cond"
       end
  | Syntax.EDot (e1, name) ->
     let ex1 = ex e1 in
     let ty = resolve_member_type (typeof ex1) name in
     EDot(ty, ex1, name)
  | Syntax.ECast (ty, e) ->
     let e = ex e in
     let ty2 = typeof e in
     ECast (ty, ty2, e)
  | Syntax.ESizeof (ty) ->
     let i = sizeof ty in
     EConst (TUInt, VInt i)
  | Syntax.ESizeofExpr (e) ->
     let i = sizeof (typeof (ex' e)) in
     EConst (TUInt, VInt i)
  | Syntax.EPadding ->
     EPadding

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
     let f (ty, e) =
       let e = ex e in
       if e = EPadding then e else
       deref_cast (ECast(ty, typeof e, e)) in
     Decl(ln, ty, name, List.map f init)

let rec st = function
  | Syntax.SNil -> SNil
  | Syntax.SBlock(x, y) ->
     let x1 = List.map dv x in
     let y1 = List.map st y in
     SBlock(x1, y1)
  | Syntax.SWhile (e, stmt) ->
     let e1 = ex e in
     let s1 = st stmt in
     SWhile (e1, s1)
  | Syntax.SDoWhile (stmt, e) ->
     let s1 = st stmt in
     let e1 = ex e in
     SDoWhile (s1, e1)
  | Syntax.SFor (e1, e2, e3, stmt) ->
     let oe1 = ex_opt e1 in
     let oe2 = ex_opt e2 in
     let oe3 = ex_opt e3 in
     let s1 = st stmt in
     SFor (oe1, oe2, oe3, s1)
  | Syntax.SIfElse (e, s1, s2) ->
     let ex1 = ex e in
     let ty1 = typeof ex1 in
     let ex1 =
       if is_real ty1 then
         fold_expr (EFEq (TInt, Ne, ex1, EConst (ty1, VFloat 0.0)))
       else ex1 in
     let st1 = st s1 in
     let st2 = st s2 in
     SIfElse (ex1, st1, st2)
  | Syntax.SReturn e ->
     begin match ex_opt e with
     | None ->
        SReturn (None)
     | Some ex1 ->
        let ty = typeof ex1 in
        if ty = !ret_ty_ref then
          SReturn (Some ex1)
        else
          SReturn (Some (ECast(!ret_ty_ref, ty, ex1)))
     end
  | Syntax.SContinue ->
     SContinue
  | Syntax.SBreak ->
     SBreak
  | Syntax.SLabel (str, stmt) ->
     let st1 = st stmt in
     SLabel (str, st1)
  | Syntax.SGoto str ->
     SGoto str
  | Syntax.SSwitch (e, stmt) ->
     let ex1 = ex e in
     let st1 = st stmt in
     SSwitch (ex1, st1)
  | Syntax.SCase e ->
     begin match ex e with
     | EConst (_, VInt i) -> SCase i
     | _ -> raise_error "case: required constant expression"
     end
  | Syntax.SDefault ->
     SDefault
  | Syntax.SExpr e ->
     let ex1 = ex e in
     SExpr ex1

let def = function
  | Syntax.DefFun (d, dlist, b) ->
     let Syntax.Decl( _, _, fname, _) = d in
     fun_name_ref := fname;
     let d1 = dv d in
     ret_ty_ref := get_ret_ty d1;
     if !ret_ty_ref != TVoid && sizeof !ret_ty_ref > 4 then
       raise_error "def : function that returns struct is not supported";
     let old_venv = !venv_ref in
     let a1 = List.map dv dlist in
     let b1 = st b in
     let ret = DefFun (d1, a1, b1) in
     venv_ref := old_venv;
     fun_name_ref := "";
     ret
  | Syntax.DefVar decl ->
     DefVar (dv decl)

let main defs =
  List.map def defs
