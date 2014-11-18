open Syntax
open Printf
let rec print_program fmt p =
  fprintf fmt "[\n";
  pp_defs fmt p;
  fprintf fmt "]\n";
and pp_defs fmt = function
  |[] ->
    fprintf fmt ""
  |x::xs ->
    pp_def fmt x;
    fprintf fmt ",\n";
    pp_defs fmt xs
and pp_def fmt = function
  | DVars (ty, dl, (a, b)) ->
     fprintf fmt "offset=%d:\nDVars(int, [%a])" a.pos_cnum pp_declars dl
  | DFun (ty, Name s, l1, st, (a, _)) ->
     fprintf fmt "offset=%d:\nDFun(int, %s, [%a], {%a})" a.pos_cnum s pp_params l1 pp_stmts st
and pp_declars fmt = function
  | [] ->
     fprintf fmt ""
  | d::ds ->
     pp_declar fmt d;
     fprintf fmt ",";
     pp_declars fmt ds
and pp_declar fmt = function
  | DeclIdent name ->
     let Name x = name in
     fprintf fmt "%s" x
  | DeclFProto (d, tlist) ->
     fprintf fmt "Fun %a (%a)" pp_declar d pp_types tlist
and pp_types fmt = function
  | [] ->
     fprintf fmt ""
  | t::ts ->
     fprintf fmt "int, %a" pp_types ts;
and pp_namelist fmt = function
  | [] ->
     fprintf fmt ""
  | (Name n)::ns ->
     fprintf fmt "%s," n;
     pp_namelist fmt ns
and pp_params fmt = function
  | [] ->
     fprintf fmt ""
  | x::xs ->
     pp_param fmt x;
     fprintf fmt ", ";
     pp_params fmt xs
and pp_param fmt (ty, Name v) =
  fprintf fmt "(int, %s)" v
and pp_stmts fmt = function
  |[] ->
    fprintf fmt ""
  |x::xs ->
    pp_stmt fmt x;fprintf fmt ", ";pp_stmts fmt xs
and pp_stmt fmt = function
  | SNil ->
     fprintf fmt ";"
  | SVars (ty, nl, (a, b)) ->
     fprintf fmt "SVars(int, [%a])" pp_namelist nl
  | SWhile (e, s) ->
     fprintf fmt "SWhile(%a, {%a})" pp_expr e pp_stmts s
  | SFor (el1, Some e2, el3, s) ->
     fprintf fmt "SFor([%a], %a, [%a], %a)" pp_exprs el1 pp_expr e2 pp_exprs el3 pp_stmts s
  | SFor (el1, None, el3, s) ->
     fprintf fmt "SFor([%a],, [%a], %a)" pp_exprs el1 pp_exprs el3 pp_stmts s
  | SIf (e, s) ->
     fprintf fmt "SIf(%a, {%a})" pp_expr e pp_stmts s
  | SIfElse (e, s1, s2) ->
     fprintf fmt "SIfElse(%a, {%a}, {%a})" pp_expr e pp_stmts s1 pp_stmts s2
  | SReturn e ->
     fprintf fmt "SReturn(%a)" pp_expr e
  | SExpr e ->
     fprintf fmt "SExpr(%a);" pp_expr e
and pp_exprs fmt= function
  | [] ->
     fprintf fmt ""
  | x::xs ->
     pp_expr fmt x;
     fprintf fmt ",";
     pp_exprs fmt xs;
and pp_expr fmt = function
  | EConst v ->
     fprintf fmt "EConst(%a)" pp_value v
  | EVar (Name s) ->
     fprintf fmt "EVar(%s)" s
  | EAdd (e1, e2) ->
     fprintf fmt "EAdd(%a, %a)" pp_expr e1 pp_expr e2
  | ESub (e1, e2) ->
     fprintf fmt "ESub(%a, %a)" pp_expr e1 pp_expr e2
  | ESubst (e1, e2) ->
     fprintf fmt "ESubst(%a, %a)" pp_expr e1 pp_expr e2
  | EMod (e1, e2) ->
     fprintf fmt "EMod(%a, %a)" pp_expr e1 pp_expr e2
  | EApp (Name s, args) ->
     fprintf fmt "EApp(%s, %a)" s pp_exprs args
  | ELt (e1, e2) ->
     fprintf fmt "ELt(%a, %a)" pp_expr e1 pp_expr e2
  | EEq (e1, e2) ->
     fprintf fmt "EEq(%a, %a)" pp_expr e1 pp_expr e2
  | ENeq (e1, e2) ->
     fprintf fmt "ENeq(%a, %a)" pp_expr e1 pp_expr e2
and pp_value fmt = function
  | VInt i ->
     fprintf fmt "VInt(%d)" i
