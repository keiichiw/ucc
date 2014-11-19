open Syntax
open Printf
exception EmitError of string

let label_num = ref 0;;
let reg_num = ref 0;;
let max_reg = ref 0;;

let param_ref = ref [];;
let param_num = ref 0;;

let buffer_ref = ref [];;

(* Access ref values*)
let reg_alloc _ =
  reg_num := !reg_num + 1;
  max_reg := (max !max_reg !reg_num);
  if !reg_num > 28 then
    raise (EmitError "register starvation!!");
  !reg_num

let regs_alloc_num x =
  reg_num := x

let reg_free a =
  reg_num := a-1

let label_create _ =
  label_num := !label_num + 1;
  !label_num

let push_buffer str =
  buffer_ref := str::!buffer_ref

let print_buffer oc name =
  let rec p_save _ =
    fprintf oc "\tpush r31\n" in
  let rec p_restore _ =
    fprintf oc "\tpop r31\n" in
  let rec p = function
    | [] -> ()
    | inst::insts ->
       if inst = "\tret\n" then
         p_restore ();
       fprintf oc "%s" inst;
       p insts in
  let buf = List.rev !buffer_ref in
  buffer_ref := [];
  fprintf oc "%s:\n" name;
  p_save ();
  p buf

let args_assoc s =
  let rec go it var = function
    | [] -> raise (TODO "variable not found")
    | (_, Name str)::xs ->
       if str = var then
         it
       else
         go (it+1) var xs in
  go 1 s !param_ref



let rec main oc = function
  | []  -> ()
  |x::xs -> emit oc x;main oc xs
and emit oc = function
  | DFun(TInt, Name name, ps,stmts, _) ->
     param_ref := ps;
     reg_num := List.length ps;
     st stmts;
     print_buffer oc name
  | _ -> raise (TODO "emit:emit")
and st = function
  | [] -> ()
  | x::xs -> st' x; st xs
and st' = function
  | SIfElse (cond, sl1, sl2) ->
     let cond_reg = ex cond in
     let lnum = label_create () in
     let endlnum = label_create () in
     push_buffer (sprintf "\tbeq r0, r%d, L%d\n" cond_reg lnum);
     st sl1;
     push_buffer (sprintf "\tbr L%d\n" endlnum);
     push_buffer (sprintf "L%d:\n" lnum);
     st sl2;
     push_buffer (sprintf "L%d:\n" endlnum);
  | SReturn exp ->
     let reg = ex exp in
     if reg != 1 then
       push_buffer (sprintf "\tmov r1, r%d\n" reg);
     push_buffer (sprintf "\tret\n");
     reg_free reg
  | _ -> raise (TODO "emit: st'")
and ex arg =
  (match arg with
   | EConst (VInt i) ->
      let ret_reg = reg_alloc () in
      push_buffer (sprintf "\tli r%d, %d\n" ret_reg i);
      ret_reg
   | EVar (Name s) ->
      args_assoc s
   | EAdd (e1, e2) ->
      (
        let ret_reg = reg_alloc () in
        let lreg = ex e1 in
        let rreg = ex e2 in
        push_buffer (sprintf "\tadd r%d, r%d, r%d\n" ret_reg lreg rreg);
        reg_free (ret_reg+1);
        ret_reg
      )
   | ESub (e1, e2) ->
      (
        let ret_reg = reg_alloc () in
        let lreg = ex e1 in
        let rreg = ex e2 in
        push_buffer (sprintf "\tsub r%d, r%d, r%d\n" ret_reg lreg rreg);
        reg_free (ret_reg+1);
        ret_reg
      )
   | ELt (e1, e2) ->
      (
        let ret_reg = reg_alloc () in
        let lreg = ex e1 in
        let rreg = ex e2 in
        push_buffer (sprintf "\tcmplt r%d, r%d, r%d\n" ret_reg lreg rreg);
        reg_free (ret_reg+1);
        ret_reg
      )
   | EApp (Name fname, exlst) ->
      (
        let rec pfun i = function
          | [] -> ()
          | e::es ->
             let reg = ex e in
             if i != reg then
               printf "\tmov r%d, r%d\n" i reg;
             pfun (i+1) es in
        let used_num = !reg_num in
        for i = 1 to used_num do (*pushes*)
          push_buffer (sprintf "\tpush r%d\n" i)
        done;
        reg_free 1;
        pfun 1 exlst;
        push_buffer (sprintf "\tcall %s\n" fname);
        regs_alloc_num used_num;
        let ret_reg = reg_alloc () in
        push_buffer (sprintf "\tmov r%d, r1\n" ret_reg);
        for i = used_num downto 1 do (*pops*)
          push_buffer (sprintf "\tpop r%d\n" i)
        done;
        ret_reg
      )
   | _ -> raise (TODO "emit: ex"));
