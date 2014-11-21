open Syntax
open Printf
exception EmitError of string

let label_num = ref 0;;
let reg_num = ref 0;;
let max_reg = ref 0;;

let param_ref = ref [];;
let param_num = ref 0;;

let buffer_ref = ref [];;

let sp_diff_ref = ref 0;;


(*
  (type, name, offset)
  offsetが負の時はレジスタ番号あらわす
 *)
let env_ref = ref [(TInt, "init", 0)];;

(* Access ref values*)

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



let resolve_var name =
  let rec go var = function
    | [] -> raise (TODO "variable not found")
    | (_, v, addr)::xs ->
       if v=var then
         addr
       else
         go var xs in
  go name !env_ref;;
let rec range a b =
  if a > b then []
  else a :: range (a+1) b;;

let push_vars fp lst =
  let len = List.length lst in
  if fp = 0 then (*レジスタに保存されている変数を追加 アドレスは負*)
    let l = List.map2 (fun (ty, Name name) i -> (ty, name, -i)) lst (range 1 len) in
    env_ref := l@(!env_ref)
  else (*ローカル変数を追加 アドレス*)
    let l = List.map2 (fun (ty, Name name) i -> (ty, name, i)) lst (range 1 len) in
    env_ref := l@(!env_ref)

let pop_vars num =
  let rec drop xs n =
    match (n, xs) with
    | (0, _) -> xs
    | (_, []) -> []
    | (_, _::ys) -> drop ys (n - 1) in
  env_ref := drop (!env_ref) num

let rec main oc = function
  | []  -> ()
  |x::xs -> emit oc x;main oc xs
and emit oc = function
  | DFun(TInt, Name name, args, b, _) ->
     param_ref := args;
     reg_num := List.length args;
     param_num := List.length args;
     push_vars 0 args;
     bl b;
     pop_vars (List.length args);
     param_num := 0;
     print_buffer oc name
  | _ -> raise (TODO "emit:emit")
and bl = function
  | Block (vars, stmts) ->
     let lvar_num = List.length vars in
     sp_diff_ref := !sp_diff_ref + lvar_num;
     if lvar_num != 0 then
       (let reg = reg_alloc () in
        push_buffer (sprintf "\tli r%d %d\n" reg lvar_num);
        push_buffer (sprintf "\tsub sp sp r%d\n" reg);
        reg_free reg
       );
     push_vars 1 (List.map (fun sv ->let SVar(x,y) = sv in (x,y)) vars);
     st stmts;
     pop_vars lvar_num;
     if lvar_num != 0 then
       (let reg = reg_alloc () in
        push_buffer (sprintf "\tli r%d %d\n" reg lvar_num);
        push_buffer (sprintf "\tadd sp sp r%d\n" reg);
        reg_free reg
       );
     sp_diff_ref := !sp_diff_ref - lvar_num
and st = function
  | [] -> ()
  | x::xs -> st' x; st xs
and st' = function
  | SNil -> ()
  | SWhile (cond, b) ->
     let lnum = label_create () in
     let endlnum = label_create () in
     push_buffer (sprintf "L%d:\n" lnum);
     let cond_reg = ex cond in
     push_buffer (sprintf "\tbeq r0, r%d, L%d\n"
                          cond_reg
                          endlnum);
     reg_free cond_reg;
     bl b;
     push_buffer (sprintf "\tbr L%d\n" endlnum);
     push_buffer (sprintf "L%d:\n" endlnum)
  | SFor(init, cond, iter, b) ->
     let lnum = label_create () in
     let endlnum = label_create () in
     (match init with
      | Some iex ->
         let temp = ex iex in reg_free temp
      | _ -> ());
     push_buffer (sprintf "L%d:\n" lnum);
     (match cond with
      | Some cex ->
         let cond_reg = ex cex in
         push_buffer (sprintf "\tbeq r0, r%d, L%d\n" cond_reg endlnum);
         reg_free cond_reg
      | _ -> ());
     bl b;
     (match iter with
      | Some itex ->
         let temp = ex itex in
         reg_free temp
      |  _ -> ());
     push_buffer (sprintf "\tbr L%d\n" endlnum);
     push_buffer (sprintf "L%d:\n" endlnum)
  | SIfElse (cond, b1, b2) ->
     let cond_reg = ex cond in
     let lnum = label_create () in
     let endlnum = label_create () in
     push_buffer (sprintf "\tbeq r0, r%d, L%d\n"
                          cond_reg
                          lnum);
     reg_free cond_reg;
     bl b1;
     push_buffer (sprintf "\tbr L%d\n" endlnum);
     push_buffer (sprintf "L%d:\n" lnum);
     bl b2;
     push_buffer (sprintf "L%d:\n" endlnum)
  | SReturn exp ->
     let reg = ex exp in
     if reg != 1 then
       push_buffer (sprintf "\tmov r1, r%d\n" reg);
     if !sp_diff_ref != 0 then
       (push_buffer (sprintf "\tli r%d, %d\n" reg !sp_diff_ref);
        push_buffer (sprintf "\tadd sp, r%d\n" reg));
     push_buffer (sprintf "\tret\n");
     reg_free reg
  | SExpr exp ->
     let temp = ex exp in
     reg_free temp
and ex arg =
  (match arg with
   | EComma (ex1, ex2) ->
      let temp = ex ex1 in
      reg_free temp;
      ex ex2
   | EConst (VInt i) ->
      let ret_reg = reg_alloc () in
      push_buffer (sprintf "\tli r%d, %d\n" ret_reg i);
      ret_reg
   | EVar (Name s) ->
      let offset = resolve_var s in
      let ret_reg = reg_alloc () in
      if offset < 0 then
        (push_buffer (sprintf "\tmov r%d, r%d\n" ret_reg (-offset));
         ret_reg)
      else
        (push_buffer (sprintf "\tmov r%d, -%d(fp)\n" ret_reg offset);
         ret_reg)
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
               push_buffer(sprintf "\tmov r%d, r%d\n" i reg);
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
