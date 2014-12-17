open Format
exception TypingError of string
exception TODO of string
exception Unreachable
type name = string
let venv_ref : (string * Type.ctype) list ref = ref [];;
let fenv_ref : (string * Type.ctype) list ref = ref [];;

(* This is initialized in main *)
let senv_ref : (int * ((name * Type.ctype) list)) list ref = ref [];;

let push_stack x env =
  env := x::!env
let resolve_var_type nm =
  let rec go nm  = function
    | [] -> Type.TInt
    | (s, ty)::_ when s=nm -> ty
    | _ :: xs -> go nm xs in
  go nm !venv_ref
let resolve_fun_type nm =
  let rec go nm  = function
    | [] -> Type.TInt
    | (s, ty)::_ when s=nm -> ty
    | _ :: xs -> go nm xs in
  go nm !fenv_ref
let resolve_member_type stct mem_name =
  match stct with
  | Type.TStruct s_id ->
     let dvs = List.assoc s_id !senv_ref in
     List.assoc mem_name dvs
  | _ -> raise Unreachable

let rec main defs =
  let go x =
    match dv x with
    | Type.DVar (ty, Type.Name n, _) -> (n, ty) in
  senv_ref := List.map
                (fun (mem,ds) -> (mem, List.map go ds))
                (List.rev !Syntax.struct_env);
  List.map (fun x -> def x) defs
and def = function
  | Syntax.DefFun (Syntax.DVar (Syntax.TFun(ty, args), Syntax.Name n, None), b) ->
     push_stack (n, typ ty) fenv_ref;
     let old_venv = !venv_ref in
     let old_fenv = !fenv_ref in
     let old_senv = !senv_ref in
     let a1 = List.map dv args in
     let b1 = st b in
     let ret = Type.DefFun (typ ty, Type.Name n, a1, b1) in
     venv_ref := old_venv;
     fenv_ref := old_fenv;
     senv_ref := old_senv;
     ret
  | Syntax.DefVar dvar ->
     Type.DefVar (dv dvar)
  | _ -> raise (TypingError "def")
and dv = function
  | Syntax.DVar(ty, Syntax.Name n, x) ->
     push_stack (n, typ ty) venv_ref;
     Type.DVar(typ ty, Type.Name n, opex x)
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
and ex = function
  | Syntax.EConst v ->
     Type.EConst (Type.TInt, vl v)
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
          | (Type.TPtr ty, i) when is_integral i ->
             Type.EPAdd (Type.TPtr ty, ex1, ex2)
          | (i, Type.TPtr ty) when is_integral i ->
             Type.EPAdd (Type.TPtr ty, ex2, ex1)
          | (ty1, ty2) when is_integral ty1 && is_integral ty2 ->
             let ty = int_conv (ty1,ty2) in
             Type.EArith (ty, Type.Add, ex1, ex2)
          | _ -> raise (TypingError "EArith: add"))
      | Syntax.Sub ->
         (match (typeof ex1, typeof ex2) with
          | (Type.TPtr ty1, Type.TPtr ty2) ->
             Type.EPDiff(Type.TInt, ex1, ex2)
          | (Type.TPtr ty1, i) when is_integral i ->
             let m_ex2 = ex (Syntax.EUnary(Syntax.Minus, e2)) in
             assert (is_integral (typeof m_ex2));
             Type.EPAdd (Type.TPtr ty1, ex1, m_ex2)
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
      | (Type.TInt, Type.TInt) ->
         Type.ERel (Type.TInt, op, ex1, ex2)
      | (Type.TUnsigned, _)
      | (_, Type.TUnsigned) ->
         raise (TypingError "relation: unsigned")
      | _ ->
         raise (TypingError "relation"))
  | Syntax.EEq (eop, e1, e2) ->
     let op  = eq_bin_op eop in
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     (match (typeof ex1, typeof ex2) with
      | (Type.TInt, Type.TInt) ->
         Type.EEq (Type.TInt, op, ex1, ex2)
      | (Type.TUnsigned, _)
      | (_, Type.TUnsigned) ->
         raise (TypingError "eq: unsigned")
      | _ ->
         raise (TypingError "eq"))
  | Syntax.ELog (lop, e1, e2) ->
     let op  = logical_bin_op lop in
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     (match (typeof ex1, typeof ex2) with
      | (Type.TInt, Type.TInt) ->
         Type.ELog (Type.TInt, op, ex1, ex2)
      | (Type.TUnsigned, _)
      | (_, Type.TUnsigned) ->
         raise (TypingError "logical: unsigned")
      | _ ->
         raise (TypingError "logical"))
  | Syntax.EUnary (op1, e1) ->
     let op = unary_op op1 in
     let ex1= ex e1 in
     (match (op, typeof ex1) with
      | (Type.LogNot, _)
      | (_, Type.TInt) ->
         Type.EUnary(Type.TInt, op, ex1)
      | (_, Type.TUnsigned) ->
         Type.EUnary(Type.TUnsigned, op, ex1)
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
     Type.EAddr (Type.TPtr (typeof ex1), ex1)
  | Syntax.EPtr e ->
     let ex1 = ex e in
     (match typeof ex1 with
      | Type.TPtr ty -> Type.EPtr (ty, ex1)
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
     let typ =  resolve_member_type (typeof ex1) nm in
     Type.EDot(typ, ex1, Type.Name nm)
  | Syntax.EArray (e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     (match typeof ex1 with
      | Type.TPtr t ->
         Type.EArray (t, ex1, ex2)
      | _ -> raise (TypingError "EArray"))
  | Syntax.ECast (ty, e) ->
     let e = ex e in
     let ty2 = typeof e in
     Type.ECast (typ ty, ty2, e)
and typ = function
  | Syntax.TInt -> Type.TInt
  | Syntax.TUnsigned -> Type.TUnsigned
  | Syntax.TStruct i ->
     Type.TStruct i
  | Syntax.TPtr ty -> Type.TPtr (typ ty)
  | Syntax.TArray (ty, sz) -> Type.TArray (typ ty,sz)
  | _ -> raise (TypingError "typ")
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
and vl = function
  | Syntax.VInt i -> Type.VInt i
and typeof' = function
  | Type.EArith  (t, _, _, _ ) -> t
  | Type.ERel    (t, _, _, _ ) -> t
  | Type.EPAdd   (t, _, _) -> t
  | Type.EPDiff  (t, _, _) -> t
  | Type.EEq     (t, _, _, _ ) -> t
  | Type.ELog    (t, _, _, _ ) -> t
  | Type.EUnary  (t, _, _) -> t
  | Type.EConst  (t, _) -> t
  | Type.EVar    (t, _) -> t
  | Type.EComma  (t, _, _) ->t
  | Type.EAssign (t, _, _) ->t
  | Type.ECall   (t, _, _) ->t
  | Type.EAddr   (t, _) ->t
  | Type.EPtr    (t, _) ->t
  | Type.ECond   (t, _, _, _) ->t
  | Type.EDot    (t, _, _) ->t
  | Type.ECast   (t, _, _) ->t
  | Type.EArray  (t, _, _) ->t
and typeof e =
  match typeof' e with
  | Type.TArray (t, _) -> Type.TPtr t
  | t -> t
and is_integral = function
  | Type.TInt
  | Type.TUnsigned -> true
  | _ -> false
and int_conv = function
  | (Type.TUnsigned, _)
  | (_, Type.TUnsigned) ->
     Type.TUnsigned
  | (Type.TInt, Type.TInt) ->
     Type.TInt
  | _ -> raise (TypingError "inv_conv")
