open Syntax
open Printf
let label_num = ref 0;;
let reg_num = ref 0;;
let param_ref = ref [];;
let param_num = ref 0;;

let reg_alloc _ =
  reg_num := !reg_num + 1;
  !reg_num

let reg_free a =
  reg_num := a-1


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
  | SReturn exp ->
     ex oc exp;
     fprintf oc "\tmov r1, r%d\n" (!reg_num+1); (*引数の分*)
     fprintf oc "\tret\n";
  | _ -> raise (TODO "emit: st'")
and ex oc arg =
  let ret_reg = reg_alloc () in
  (match arg with
   | EConst (VInt i) ->
      fprintf oc "\tli r%d, %d\n" ret_reg i
   | EAdd (e1, e2) ->
      (
        let lreg = reg_alloc () in
        let rreg = reg_alloc () in
        let nexreg = !reg_num + 1 in
        ex oc e1;
        fprintf oc "\tmov r%d, r%d\n" lreg nexreg;
        ex oc e2;
        fprintf oc "\tmov r%d, r%d\n" rreg nexreg;
        fprintf oc "\tadd r%d, r%d, r%d\n" ret_reg lreg rreg
      )
   | _ -> raise (TODO "emit: ex"));
  reg_free ret_reg
