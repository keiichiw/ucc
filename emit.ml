open Syntax
open Printf
let label_num = ref 0;; (*基本的に1以降を使う*)
let reg_num = ref 1;; (*基本的に2以降を使う*)
let param_num = ref 0;;
let rec main oc = function
  | []  -> fprintf oc ""
  |x::xs -> emit oc x;main oc xs
and emit oc = function
  | DFun(TInt, Name name, ps,stmts, _) ->
     fprintf oc "%s:\n" name;
     st oc stmts
  | _ -> raise (TODO "emit:emit")
and st oc = function
  | [] -> fprintf oc ""
  | x::xs -> st' oc x; st oc xs
and st' oc = function
  | SReturn exp ->
     ex oc exp;
     fprintf oc "\tret\n";
  | _ -> raise (TODO "emit: st'")
and ex oc arg =
  let start_num = !reg_num in
  (match arg with
   | EConst (VInt i) -> fprintf oc "\tli r1, %d\n" i
   | EAdd (e1, e2) ->
      (
        ex oc e1;
        let lreg = !reg_num + 1 in
        reg_num := !reg_num + 1;
        fprintf oc "\tmov r%d, r1\n" lreg;
        ex oc e2;
        let rreg = !reg_num + 1 in
        reg_num := !reg_num + 1;
        fprintf oc "\tmov r%d, r1\n" rreg;
        fprintf oc "\tadd r1, r%d, r%d\n" lreg rreg
      )
   | _ -> raise (TODO "emit: ex"));
  reg_num := start_num
