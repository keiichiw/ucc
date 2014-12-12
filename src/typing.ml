open Format
exception TypingError of string
exception TODO of string
exception Unreachable
let venv_ref : (string * Type.ctype) list ref = ref [];;
let fenv_ref : (string * Type.ctype) list ref = ref [];;
let senv_ref : (string * (Type.dvar list)) list ref = ref [];;

let rec assoc nm  = function
  | [] -> raise (TypingError (sprintf "assoc \'%s\' not found" nm))
  | (s, ty)::_ when s=nm -> ty
  | _ :: xs -> assoc nm xs
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
  let go1 = function
    | Type.DVar (ty, Type.Name n, _) -> (ty, n)
    | _ -> raise Unreachable in
  let rec go2 mem = function
    | [] ->
       raise (TypingError (sprintf "member \'%s\' not found" mem))
    | x::xs ->
       (match (go1 x) with
        | (ty, name) when name=mem -> ty
        | _ -> go2 mem xs) in
  match stct with
  | Type.TStruct (Some (Type.Name struct_name), _) ->
     let dvs = assoc struct_name !senv_ref in
     go2 mem_name dvs
  | _ -> raise Unreachable

let rec main defs =
  List.map (fun x -> def x) defs
and def = function
  | Syntax.DefFun (Syntax.DVar (Syntax.TFun(ty, args), Syntax.Name n, None), b) ->
     push_stack (n, typ ty) fenv_ref;
     let old_venv = !venv_ref in
     let old_fenv = !fenv_ref in
     let old_senv = !senv_ref in
     let a1 = List.map dv args in
     let b1 = bl b in
     let ret = Type.DefFun (typ ty, Type.Name n, a1, b1) in
     venv_ref := old_venv;
     fenv_ref := old_fenv;
     senv_ref := old_senv;
     ret
  | Syntax.DefVar dvar ->
     Type.DefVar (dv dvar)
  | _ -> raise (TypingError "def")
and bl = function
  | Syntax.Block (dvar, stmts) ->
     let old_venv = !venv_ref in
     let old_fenv = !fenv_ref in
     let old_senv = !senv_ref in
     let v = List.map dv dvar in
     let s = List.map st stmts in
     let t = Type.Block(v, s) in
     venv_ref := old_venv;
     fenv_ref := old_fenv;
     senv_ref := old_senv;
     t
and dv = function
  | Syntax.DVar(ty, Syntax.Name n, x) ->
     push_stack (n, typ ty) venv_ref;
     Type.DVar(typ ty, Type.Name n, opex x)
  | Syntax.DStruct(Syntax.Name n, dvars) ->
     let dvlist = List.map dv dvars in
     push_stack (n, dvlist) senv_ref;
     Type.DStruct(Type.Name n, dvlist)
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
      | (Type.TInt, Type.TInt) ->
         Type.EAdd (Type.TInt, ex1, ex2)
      | (Type.TPtr ty, Type.TInt)
      | (Type.TInt, Type.TPtr ty)
      | (Type.TInt, Type.TArray (ty, _))
      | (Type.TArray (ty, _), Type.TInt) ->
         Type.EAdd (Type.TPtr ty, ex1, ex2)
      | _ ->
       raise (TypingError "add"))
  | Syntax.ESub (e1, e2) ->
     let ex1 = ex e1 in
     let ty1 = typeof ex1 in
     let ex2 = ex e2 in
     let ty2 = typeof ex2 in
     (match (ty1, ty2) with
      | (Type.TInt, Type.TInt) ->
         Type.ESub (Type.TInt, ex1, ex2)
      | (Type.TPtr ty, Type.TInt)
      | (Type.TInt, Type.TPtr ty)
      | (Type.TInt, Type.TArray (ty, _))
      | (Type.TArray (ty, _), Type.TInt) ->
         Type.ESub (Type.TPtr ty, ex1, ex2)
      | _ ->
       raise (TypingError "sub"))
  | Syntax.EShift (e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     if (typeof ex1) = (typeof ex2) && (typeof ex1) = Type.TInt then
       Type.EShift (Type.TInt, ex1, ex2)
     else
       raise (TypingError "shift")
  | Syntax.EAssign (e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     Type.EAssign (typeof ex2, ex1, ex2)
  | Syntax.EApp (e1, elist) ->
     let ex1 = ex e1 in
     Type.EApp (typeof ex1, ex1, List.map ex elist)
  | Syntax.ELe (e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     if (typeof ex1) = (typeof ex2) then
       Type.ELe (Type.TInt, ex1, ex2)
     else
       raise (TypingError "le")
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
     ex (Syntax.EPtr (Syntax.EAdd (e1, e2)))
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
and typ = function
  | Syntax.TInt -> Type.TInt
  | Syntax.TStruct (Some (Syntax.Name name), Some dvars) ->
     Type.TStruct (Some (Type.Name name), Some (List.map dv dvars))
  | Syntax.TStruct (Some (Syntax.Name name), None) ->
     Type.TStruct (Some (Type.Name name), None)
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
