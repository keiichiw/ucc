open Format
open Syntax
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
  | DVar (ty, Name s, e) ->
     fprintf fmt "DVar(int, %s, %a)" s pp_expr e
  | DFun (ty, Name s, l1, st) ->
     fprintf fmt "DFun(int, %s, [%a], %a)" s pp_params l1 pp_stmts st
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
    pp_stmt fmt x;pp_stmts fmt xs
and pp_stmt fmt = function
  | SNil ->
     fprintf fmt ";"
  | SWhile (e, s) ->
     fprintf fmt "SWhile(%a, %a)" pp_expr e pp_stmts s
  | SIf (e, s) ->
     fprintf fmt "SIf(%a, %a)" pp_expr e pp_stmts s
  | SIfElse (e, s1, s2) ->
     fprintf fmt "SIfElse(%a, %a, %a)" pp_expr e pp_stmts s1 pp_stmts s2
  | SReturn e ->
     fprintf fmt "SReturn(%a)" pp_expr e
  | SExpr e ->
     fprintf fmt "SExpr(%a);" pp_expr e
and pp_exprs fmt= function
  | [] ->
     fprintf fmt ""
  | x::xs ->
     pp_expr fmt x;pp_exprs fmt xs;
and pp_expr fmt = function
  | EConst v ->
     fprintf fmt "EConst(%a)" pp_value v
  | EVar (Name s) ->
     fprintf fmt "EVar(%s)" s
  | EAdd (e1, e2) ->
     fprintf fmt "EAdd(%a, %a)" pp_expr e1 pp_expr e2
  | ESub (e1, e2) ->
     fprintf fmt "ESub(%a, %a)" pp_expr e1 pp_expr e2
  | EMod (e1, e2) ->
     fprintf fmt "EMod(%a, %a)" pp_expr e1 pp_expr e2
  | EApp (Name s, args) ->
     fprintf fmt "EApp(%s, %a)" s pp_exprs args
  | ELt (e1, e2) ->
     fprintf fmt "ELt(%a, %a)" pp_expr e1 pp_expr e2
  | EEq (e1, e2) ->
     fprintf fmt "EEq(%a, %a)" pp_expr e1 pp_expr e2
and pp_value fmt = function
  | VInt i ->
     fprintf fmt "VInt(%d)" i
