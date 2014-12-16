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
  | Syntax.SCase e ->
     let ex1 = ex e in
     Type.SCase (ex1)
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
  | Syntax.EAdd (e1, e2) ->
     let ex1 = ex e1 in
     let ty1 = typeof ex1 in
     let ex2 = ex e2 in
     let ty2 = typeof ex2 in
     (match (ty1, ty2) with
      | (Type.TStruct ty, _)
      | (_, Type.TStruct ty) ->
         raise (TypingError "add: struct")
      | (Type.TPtr ty, _)
      | (_, Type.TPtr ty)
      | (_, Type.TArray (ty, _))
      | (Type.TArray (ty, _), _) ->
         Type.EAdd (Type.TPtr ty, ex1, ex2)
      | (_, Type.TUnsigned)
      | (Type.TUnsigned, _) ->
         Type.EAdd (Type.TUnsigned, ex1, ex2)
      | (Type.TInt, Type.TInt) ->
         Type.EAdd (Type.TInt, ex1, ex2)
      | _ ->
         raise (TypingError "add: unreachable"))
  | Syntax.ESub (e1, e2) ->
     let ex1 = ex e1 in
     let ty1 = typeof ex1 in
     let ex2 = ex e2 in
     let ty2 = typeof ex2 in
     (match (ty1, ty2) with
      | (Type.TStruct ty, _)
      | (_, Type.TStruct ty) ->
         raise (TypingError "sub: struct")
      | (Type.TPtr ty, _)
      | (_, Type.TPtr ty)
      | (_, Type.TArray (ty, _))
      | (Type.TArray (ty, _), _) ->
         Type.ESub (Type.TPtr ty, ex1, ex2)
      | (_, Type.TUnsigned)
      | (Type.TUnsigned, _) ->
         Type.ESub (Type.TUnsigned, ex1, ex2)
      | (Type.TInt, Type.TInt) ->
         Type.ESub (Type.TInt, ex1, ex2)
      | _ ->
         raise (TypingError "sub: unreachable"))
  | Syntax.EShift (e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     (match (typeof ex1, typeof ex2) with
      | (Type.TInt, Type.TUnsigned)
      | (Type.TInt, Type.TInt) ->
         Type.EShift (Type.TInt, ex1, ex2)
      | (Type.TUnsigned, Type.TInt)
      | (Type.TUnsigned, Type.TUnsigned) ->
         Type.EShift (Type.TUnsigned, ex1, ex2)
      | _ ->
         raise (TypingError "shift"))
  | Syntax.EAssign (e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     Type.EAssign (typeof ex1, ex1, ex2)
  | Syntax.EApp (e1, elist) ->
     let ex1 = ex e1 in
     Type.EApp (typeof ex1, ex1, List.map ex elist)
  | Syntax.ELe (e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     (match (typeof ex1, typeof ex2) with
      | (Type.TInt, Type.TInt) ->
         Type.ELe (Type.TInt, ex1, ex2)
      | (Type.TUnsigned, _)
      | (_, Type.TUnsigned) ->
         raise (TypingError "le: unsigned")
      | _ ->
         raise (TypingError "le"))
  | Syntax.EEq (e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     if (typeof ex1) = (typeof ex2) then
       Type.EEq (Type.TInt, ex1, ex2)
     else
       raise (TypingError "eq")
  | Syntax.ENeq (e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     if (typeof ex1) = (typeof ex2) then
       Type.ENeq (Type.TInt, ex1, ex2)
     else
       raise (TypingError "neq")
  | Syntax.EAddr e ->
     let ex1 = ex e in
     Type.EAddr (Type.TPtr (typeof ex1), ex1)
  | Syntax.EPtr e ->
     let ex1 = ex e in
     (match typeof ex1 with
      | Type.TPtr ty -> Type.EPtr (ty, ex1)
      | _ -> raise (TypingError "ptr"))
  | Syntax.EArray (e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     (match typeof ex1 with
      | Type.TPtr ty
      | Type.TArray (ty,_) ->
         Type.EArray (ty, ex1, ex2)
      | _ -> raise (TypingError "typing: earray"))
  | Syntax.ECond (e1, e2, e3) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     let ex3 = ex e3 in
     if (typeof ex2) = (typeof ex3) then
       Type.ECond (typeof ex2, ex1, ex2, ex3)
     else
       raise (TypingError "cond")
  | Syntax.EAnd (e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     (match (typeof ex1, typeof ex2) with
      | (Type.TInt, Type.TInt) ->
         Type.EAnd (Type.TInt, ex1, ex2)
      | _ -> raise (TypingError "and"))
  | Syntax.EOr (e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     (match (typeof ex1, typeof ex2) with
      | (Type.TInt, Type.TInt) ->
         Type.EOr (Type.TInt, ex1, ex2)
      | _ -> raise (TypingError "or"))
  | Syntax.EDot (e1, Syntax.Name nm) ->
     let ex1 = ex e1 in
     let typ =  resolve_member_type (typeof ex1) nm in
     Type.EDot(typ, ex1, Type.Name nm)
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
and vl = function
  | Syntax.VInt i -> Type.VInt i
and typeof = function
  | Type.EConst (t, _) -> t
  | Type.EVar   (t, _) -> t
  | Type.EComma (t, _, _) ->t
  | Type.EAdd   (t, _, _) ->t
  | Type.EShift (t, _, _) ->t
  | Type.ESub   (t, _, _) ->t
  | Type.EAssign(t, _, _) ->t
  | Type.EApp   (t, _, _) ->t
  | Type.ELe    (t, _, _) ->t
  | Type.EEq    (t, _, _) ->t
  | Type.ENeq   (t, _, _) ->t
  | Type.EAddr  (t, _) ->t
  | Type.EPtr   (t, _) ->t
  | Type.ECond  (t, _, _, _) ->t
  | Type.EAnd   (t, _, _) ->t
  | Type.EOr    (t, _, _) ->t
  | Type.EDot   (t, _, _) ->t
  | Type.EArray (t, _, _) ->t
  | Type.ECast  (t, _, _) ->t
