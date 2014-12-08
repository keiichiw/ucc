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
  assoc nm (List.rev !venv_ref)
let resolve_fun_type nm =
  assoc nm (List.rev !fenv_ref)
let resolve_member_type stct mem_name =
  let go1 = function
    | Type.DVar (ty, Type.Name n, _) -> (ty, n)
    | Type.DArray (ty, Type.Name n, _) -> (ty, n)
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
     (match assoc struct_name (List.rev !senv_ref) with
      | dvs ->
         go2 mem_name dvs
      |_ -> raise (TypingError "resolve member"))
  | _ -> raise Unreachable

let rec main defs =
  List.map (fun x -> def x) defs
and def = function
  | Syntax.DefFun (ty, Syntax.Name n, args, b, loc) ->
     push_stack (n, typ ty) fenv_ref;
     Type.DefFun (typ ty, Type.Name n, List.map dv args, bl b, loc)
  | Syntax.DefVar dvar ->
     Type.DefVar (dv dvar)
and bl = function
  | Syntax.Block (dvar, stmts) ->
     let old_venv = !venv_ref in
     let old_fenv = !fenv_ref in
     let old_senv = !senv_ref in
     let t = Type.Block(List.map dv dvar, List.map st stmts) in
     venv_ref := old_venv;
     fenv_ref := old_fenv;
     senv_ref := old_senv;
     t
and dv = function
  | Syntax.DVar(ty, Syntax.Name n, x) ->
     push_stack (n, typ ty) venv_ref;
     Type.DVar(typ ty, Type.Name n, opex x)
  | Syntax.DArray(ty, Syntax.Name n, x) ->
     push_stack (n, Type.TArray (typ ty)) venv_ref;
     Type.DArray(typ ty, Type.Name n,x)
  | Syntax.DStruct(Name n, dvars) ->
     let dvlist = List.map dv dvars in
     push_stack (n, dvlist) senv_ref;
     Type.DStruct(Name n, dvlist)
and st = function
  | Syntax.SNil -> Type.SNil
  | Syntax.SBlock(x, y) ->
     Type.SBlock(List.map dv x, List.map st y)
  | Syntax.SWhile (e, stmt) ->
     Type.SWhile (ex e, st stmt)
  | Syntax.SDoWhile (stmt, e) ->
     Type.SDoWhile (st stmt, ex e)
  | Syntax.SFor (e1, e2, e3, stmt) ->
     Type.SFor (opex e1, opex e2, opex e3, st stmt)
  | Syntax.SIfElse (e, s1, s2) ->
     Type.SIfElse (ex e, st s1, st s2)
  | Syntax.SReturn e ->
     Type.SReturn (ex e)
  | Syntax.SContinue ->
     Type.SContinue
  | Syntax.SBreak ->
     Type.SBreak
  | Syntax.SLabel (str, stmt) ->
     Type.SLabel (str, st stmt)
  | Syntax.SGoto str ->
     Type.SGoto str
  | Syntax.SSwitch (e, stmt) ->
     Type.SSwitch (ex e, st stmt)
  | Syntax.SCase e ->
     Type.SCase (ex e)
  | Syntax.SDefault ->
     Type.SDefault
  | Syntax.SExpr e ->
     Type.SExpr (ex e)
and opex = function
  | Some x -> Some (ex x)
  | None -> None
and ex = function
  | Syntax.EConst v ->
     Type.EConst (Type.TInt, vl v)
  | Syntax.EVar (Name n)->
     Type.EVar (resolve_var_type n, Name n)
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
      | (Type.TPtr Type.TInt, Type.TInt)
      | (Type.TInt, Type.TPtr Type.TInt) ->
         Type.EAdd (Type.TPtr Type.TInt, ex1, ex2)
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
      | (Type.TPtr Type.TInt, Type.TInt)
      | (Type.TInt, Type.TPtr Type.TInt) ->
         Type.ESub (Type.TPtr Type.TInt, ex1, ex2)
      | _ ->
       raise (TypingError "sub"))
  | Syntax.EShift (e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     if (typeof ex1) = (typeof ex2) && (typeof ex1) = Type.TInt then
       Type.EShift (Type.TInt, ex1, ex2)
     else
       raise (TypingError "shift")
  | Syntax.ESubst (e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     Type.ESubst (typeof ex2, ex1, ex2)
  | Syntax.EApp (Name name, elist) ->
     let ftype = resolve_fun_type name in
     Type.EApp (ftype, Name name, List.map ex elist)
  | Syntax.ELe (e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     if (typeof ex1) = (typeof ex2) then
       Type.ELe (TInt, ex1, ex2)
     else
       raise (TypingError "le")
  | Syntax.EEq (e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     if (typeof ex1) = (typeof ex2) then
       Type.EEq (TInt, ex1, ex2)
     else
       raise (TypingError "eq")
  | Syntax.ENeq (e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     if (typeof ex1) = (typeof ex2) then
       Type.ENeq (TInt, ex1, ex2)
     else
       raise (TypingError "neq")
  | Syntax.EAddr e ->
     let ex1 = ex e in
     Type.EAddr (Type.TPtr (typeof ex1), ex1)
  | Syntax.EPtr e ->
     let ex1 = ex e in
     (match typeof ex1 with
      | TPtr ty -> Type.EPtr (ty, ex1)
      | _ -> raise (TypingError "ptr"))
  | Syntax.EArray (e1, e2) ->
     let ex1 = ex e1 in
     let ex2 = ex e2 in
     (match (typeof ex1, typeof ex2) with
      | (Type.TArray ty1, Type.TInt) ->
         Type.EArray (ty1, ex1, ex2)
      | (Type.TPtr ty1, Type.TInt) ->
         Type.EArray (ty1, ex1, ex2)
      | _ -> raise (TypingError "array"))
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
         Type.EAnd (Type.TInt, ex1, ex2)
      | _ -> raise (TypingError "and"))
  | Syntax.EDot (e1, Syntax.Name nm) ->
     let ex1 = ex e1 in
     let typ =  resolve_member_type (typeof ex1) nm in
     Type.EDot(typ, ex1, Name nm)
  | _ -> raise (TypingError "error")
and typ = function
  | Syntax.TInt -> Type.TInt
  | Syntax.TPtr x   -> Type.TPtr (typ x)
  | Syntax.TArray x -> Type.TArray (typ x)
  | Syntax.TStruct (Some (Syntax.Name name), Some dvars) ->
     Type.TStruct (Some (Type.Name name), Some (List.map dv dvars))
  | _ -> raise (TypingError "typ")
and vl = function
  | Syntax.VInt i -> Type.VInt i
and typeof = function
  | Type.EConst (t, _) -> t
  | Type.EVar   (t, _) ->t
  | Type.EComma (t, _, _) ->t
  | Type.EAdd   (t, _, _) ->t
  | Type.EShift (t, _, _) ->t
  | Type.ESub   (t, _, _) ->t
  | Type.ESubst (t, _, _) ->t
  | Type.EApp   (t, _, _) ->t
  | Type.ELe    (t, _, _) ->t
  | Type.EEq    (t, _, _) ->t
  | Type.ENeq   (t, _, _) ->t
  | Type.EAddr  (t, _) ->t
  | Type.EPtr   (t, _) ->t
  | Type.EArray (t, _, _) ->t
  | Type.ECond  (t, _, _, _) ->t
  | Type.EAnd   (t, _, _) ->t
  | Type.EOr    (t, _, _) ->t
  | Type.EDot   (t, _, _) ->t
