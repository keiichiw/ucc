open Syntax
open Printf
let label_num = ref 0;;
let reg_num = ref 0;;
let param_ref = ref [];;
let param_num = ref 0;;

let args_assoc s =
  let rec go it var = function
    | [] -> raise (TODO "variable not found")
    | (_, Name str)::xs ->
       if str = var then
         it
       else
         go (it+1) var xs in
  go 1 s !param_ref


let reg_alloc _ =
  reg_num := !reg_num + 1;
  !reg_num

let reg_free a =
  reg_num := a-1
let label_create _ =
  label_num := !label_num + 1;
  !label_num

let rec main oc = function
  | []  -> fprintf oc ""
  |x::xs -> emit oc x;main oc xs
and emit oc = function
  | DFun(TInt, Name name, ps,stmts, _) ->
     param_ref := ps;
     reg_num := List.length ps;
     fprintf oc "%s:\n" name;
     st oc stmts
  | _ -> raise (TODO "emit:emit")
and st oc = function
  | [] -> fprintf oc ""
  | x::xs -> st' oc x; st oc xs
and st' oc = function
  | SIfElse (cond, sl1, sl2) ->
     let cond_reg = ex oc cond in
     let lnum = label_create () in
     let endlnum = label_create () in
     fprintf oc "\tbeq r0, r%d, L%d\n" cond_reg lnum;
     st oc sl1;
     fprintf oc "\tbr L%d\n" endlnum;
     fprintf oc "L%d:\n" lnum;
     st oc sl2;
     fprintf oc "L%d:\n" endlnum;
  | SReturn exp ->
     let reg = ex oc exp in
     if reg != 1 then
       fprintf oc "\tmov r1, r%d\n" reg;
     fprintf oc "\tret\n";
     reg_free reg
  | _ -> raise (TODO "emit: st'")
and ex oc arg =
  (match arg with
   | EConst (VInt i) ->
      let ret_reg = reg_alloc () in
      fprintf oc "\tli r%d, %d\n" ret_reg i;
      ret_reg
   | EVar (Name s) ->
      args_assoc s
   | EAdd (e1, e2) ->
      (
        let ret_reg = reg_alloc () in
        let lreg = ex oc e1 in
        let rreg = ex oc e2 in
        fprintf oc "\tadd r%d, r%d, r%d\n" ret_reg lreg rreg;
        reg_free (ret_reg+1);
        ret_reg
      )
   | _ -> raise (TODO "emit: ex"));
