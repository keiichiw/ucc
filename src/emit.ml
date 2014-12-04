open Syntax
open Printf
exception EmitError of string
exception Unreachable
type storageplace =
  | Reg of int (*register*)
  | Mem of int (*memory*)
  | Ptr of storageplace (*pointer*)

let created_label_num = ref 0;;
let using_reg_num = ref 0;;
let max_reg_num = ref 0;;
let buffer_ref : string list ref = ref [];;
let sp_diff_ref = ref 0;;
let con_stack : int list ref = ref [];;
let brk_stack : int list ref = ref [];;
let sp_move_stack : int list ref = ref [];;
let for_continue_flg_ref = ref 0;;
let env_ref : (ctype * string * storageplace) list ref = ref [];;

(* Access ref values*)
let push_buffer str =
  buffer_ref := str::!buffer_ref

let print_buffer oc name =
  fprintf oc ".global %s\n%s:\n" name name;
  let buf = List.rev !buffer_ref in
  (* don't insert halt directly. the way of handling halt can be changed in the future *)
  let buf = (if name = "main" then
               buf @ [ "\tmov $1, $0\n"; "\tret\n" ]
             else
               buf) in
  let _ = List.map
            (fun s -> if name = "main" && s = "\tret\n" then
                        fprintf oc "\thalt\n"
                      else
                        fprintf oc "%s" s)
            buf in
  buffer_ref := []


let reg_alloc _ =
  using_reg_num := !using_reg_num + 1;
  max_reg_num := (max !max_reg_num !using_reg_num);
  if !using_reg_num > 28 then
    raise (EmitError "register starvation!!");
  !using_reg_num

let regs_alloc_num x =
  using_reg_num := x

let reg_free a =
  using_reg_num := a-1

let label_create _ =
  created_label_num := !created_label_num + 1;
  !created_label_num


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

let push_args args = (* add args in env *)
  let rec go i = function
    | [] -> ()
    | (DVar (ty, Name name, _))::xs ->
       env_ref := (ty, name, Reg i)::!env_ref;
       go (i+1) xs
    | _ -> raise (EmitError "array can't be an argument of function.") in
  go 1 args

let push_local_vars vars ex = (* add local vars in env *)
  let go arrList = function
    | DVar (ty, Name name, x) ->
       sp_diff_ref := !sp_diff_ref + 1;
       env_ref := (ty, name, Mem !sp_diff_ref)::!env_ref;
       (match x with
        | None ->
           ()
        | Some exp ->
           (let reg = ex exp in
            push_buffer (sprintf "\tmov [$bp-%d], $%d\n" !sp_diff_ref reg);
            reg_free reg));
       arrList
    | DArray (ty, Name name, sz) ->
       sp_diff_ref := !sp_diff_ref + 1;
       env_ref := (TPtr ty, name, Mem !sp_diff_ref)::!env_ref;
       (Mem !sp_diff_ref, sz)::arrList in
  let go2 = function
    | (Mem i, sz) ->
       let reg = reg_alloc () in
       push_buffer (sprintf "\tsub $%d, $bp, %d\n" reg (!sp_diff_ref+1));
       push_buffer (sprintf "\tmov [$bp-%d], $%d\n" i reg);
       reg_free reg;
       sp_diff_ref := !sp_diff_ref + sz
    | _ -> raise Unreachable in
  let arrVars = List.fold_left go [] vars in
  let _ = List.map go2 (List.rev arrVars) in
  ()

let pop_args num =
  let rec drop xs n =
    match (n, xs) with
    | (0, _) -> xs
    | (_, (_,_,Reg _)::ys) -> drop ys (n - 1)
    | (_, (_,_,Mem _)::ys) ->
       raise (EmitError "local vars can't be removed")
    | _ ->
       raise (EmitError "pop_local_args") in
  env_ref := drop (!env_ref) num

let pop_local_vars num =
  let rec drop xs n =
    match (n, xs) with
    | (0, _) -> xs
    | (_, []) -> []
    | (_, (_,_,Mem _)::ys) -> drop ys (n - 1)
    | (_, (_,_,Reg _)::ys) ->
       raise (EmitError "args can't be removed")
    | _ ->
       raise (EmitError "pop_local_vars") in
  env_ref := drop (!env_ref) num;
  sp_diff_ref := !sp_diff_ref - num


let count_sp_move vars =
  let go i = function
    | DVar _ -> i+1
    | DArray (_, _, sz) -> i+1+sz in
  List.fold_left go 0 vars


let stack_push stack i =
  stack := (i::!stack)
let stack_pop stack =
  stack := (List.tl !stack)


(*emit main*)
let rec main oc defs =
  let _ = List.map (fun x -> emit oc x) defs in
  ()
and emit oc = function
  | DefFun(ty, Name name, args, b, _) ->
     using_reg_num := List.length args;
     push_args args;
     bl b;
     pop_args (List.length args);
     print_buffer oc name
and bl = function
  | Block (vars, stmts) ->
     let sp_move = count_sp_move vars in
     stack_push sp_move_stack sp_move;
     if sp_move != 0 then
       push_buffer (sprintf "\tsub $sp, $sp, %d\n" sp_move);
     push_local_vars vars ex;
     st stmts;
     pop_local_vars (List.length vars);
     stack_pop sp_move_stack;
     if sp_move != 0 then
       push_buffer (sprintf "\tadd $sp, $sp, %d\n" sp_move)
and st = function
  | [] -> ()
  | x::xs -> st' x; st xs
and st' = function
  | SNil -> ()
  | SWhile (cond, b) ->
     let beginlabel = label_create () in
     let endlabel = label_create () in
     stack_push con_stack beginlabel;
     stack_push brk_stack endlabel;
     push_buffer (sprintf "L%d:\n" beginlabel);
     let cond_reg = ex cond in
     push_buffer (sprintf "\tbeq $0, $%d, L%d\n"
                          cond_reg
                          endlabel);
     reg_free cond_reg;
     bl b;
     push_buffer (sprintf "\tbr L%d\n" beginlabel);
     push_buffer (sprintf "L%d:\n" endlabel);
     stack_pop con_stack;
     stack_pop brk_stack
  | SDoWhile (b, cond) ->
     let beginlabel = label_create () in
     let condlabel = label_create () in
     let endlabel = label_create () in
     stack_push con_stack condlabel;
     stack_push brk_stack endlabel;
     push_buffer (sprintf "L%d:\n" beginlabel);
     let continue_flg = !for_continue_flg_ref in
     for_continue_flg_ref := 0;
     bl b;
     if !for_continue_flg_ref = 1 then
       push_buffer (sprintf "L%d:\n" condlabel);
     for_continue_flg_ref := continue_flg;
     let cond_reg = ex cond in
     push_buffer (sprintf "\tbeq $0, $%d, L%d\n"
                          cond_reg
                          endlabel);
     push_buffer (sprintf "\tbr L%d\n" beginlabel);
     push_buffer (sprintf "L%d:\n" endlabel);
     reg_free cond_reg;
     stack_pop con_stack;
     stack_pop brk_stack
  | SFor(init, cond, iter, b) ->
     let startlnum = label_create () in
     let iterlnum = label_create () in
     let endlnum = label_create () in
     stack_push con_stack iterlnum;
     stack_push brk_stack endlnum;
     (match init with
      | Some iex ->
         let temp = ex iex in reg_free temp
      | _ -> ());
     push_buffer (sprintf "L%d:\n" startlnum);
     (match cond with
      | Some cex ->
         let cond_reg = ex cex in
         push_buffer (sprintf "\tbeq $0, $%d, L%d\n" cond_reg endlnum);
         reg_free cond_reg
      | _ -> ());
     let continue_flg = !for_continue_flg_ref in
     for_continue_flg_ref := 0;
     bl b;
     if !for_continue_flg_ref = 1 then
       push_buffer (sprintf "L%d:\n" iterlnum);
     for_continue_flg_ref := continue_flg;
     (match iter with
      | Some itex ->
         let temp = ex itex in
         reg_free temp
      |  _ -> ());
     push_buffer (sprintf "\tbr L%d\n" startlnum);
     push_buffer (sprintf "L%d:\n" endlnum);
     stack_pop con_stack;
     stack_pop brk_stack
  | SIfElse (cond, b1, b2) ->
     let cond_reg = ex cond in
     let lnum = label_create () in
     let endlnum = label_create () in
     push_buffer (sprintf "\tbeq $0, $%d, L%d\n"
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
     if !sp_diff_ref != 0 then
       (let temp = reg_alloc () in
        push_buffer (sprintf "\tmov $%d, %d\n" temp !sp_diff_ref);
        push_buffer (sprintf "\tadd $sp, $sp, $%d\n" temp);
        reg_free temp
       );
     if reg != 1 then
       push_buffer (sprintf "\tmov $1, $%d\n" reg);
     push_buffer (sprintf "\tret\n");
     reg_free reg
  | SContinue ->
     let lbl = (List.hd !con_stack) in
     push_buffer (sprintf "\tbr L%d\n" lbl);
     for_continue_flg_ref := 1
  | SBreak ->
     let lbl = (List.hd !brk_stack) in
     let sp_d = (List.hd !sp_move_stack) in
     push_buffer (sprintf "\tadd $sp, $sp, %d\n" sp_d);
     push_buffer (sprintf "\tbr L%d\n" lbl)
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
      push_buffer (sprintf "\tmov $%d, %d\n" ret_reg i);
      ret_reg
   | EVar (Name s) ->
      (let ret_reg = reg_alloc () in
       match resolve_var s with
       | Reg i ->
          push_buffer (sprintf "\tmov $%d, $%d\n" ret_reg i);
          ret_reg
       | Mem offset ->
          push_buffer (sprintf "\tmov $%d, [$bp-%d]\n" ret_reg offset);
          ret_reg
       | _ -> raise Unreachable)
   | ECond (c, t, e) ->
      (
        let ret_reg = reg_alloc () in
        let lelse = label_create () in
        let lend = label_create () in
        let creg = ex c in
        push_buffer (sprintf "\tbeq $%d, $0, L%d\n" creg lelse);
        reg_free creg;
        let treg = ex t in
        push_buffer (sprintf "\tmov $%d, $%d\n" ret_reg treg);
        push_buffer (sprintf "\tbr L%d\n" lend);
        reg_free treg;
        push_buffer (sprintf "L%d:\n" lelse);
        let ereg = ex e in
        push_buffer (sprintf "\tmov $%d, $%d\n" ret_reg ereg);
        push_buffer (sprintf "L%d:\n" lend);
        reg_free (ret_reg+1);
        ret_reg
      )
   | EAnd (e1, e2) ->
      (
        let ret_reg = reg_alloc () in
        let l1 = label_create () in
        let l2 = label_create () in
        let lreg = ex e1 in
        push_buffer (sprintf "\tbeq $%d, $0, L%d\n" lreg l1);
        reg_free lreg;
        let rreg = ex e2 in
        push_buffer (sprintf "\tmov $%d, $%d\n" ret_reg rreg);
        push_buffer (sprintf "\tbr L%d\n" l2);
        reg_free rreg;
        push_buffer (sprintf "L%d:\n" l1);
        push_buffer (sprintf "\tmov $%d, $0\n" ret_reg);
        push_buffer (sprintf "L%d:\n" l2);
        ret_reg
      )
   | EOr (e1, e2) ->
      (
        let ret_reg = reg_alloc () in
        let l1 = label_create () in
        let l2 = label_create () in
        let lreg = ex e1 in
        push_buffer (sprintf "\tbeq $%d, $0, L%d\n" lreg l1);
        push_buffer (sprintf "\tmov $%d, $%d\n" ret_reg lreg);
        reg_free lreg;
        push_buffer (sprintf "\tbr L%d\n" l2);
        push_buffer (sprintf "L%d:\n" l1);
        let rreg = ex e2 in
        push_buffer (sprintf "\tmov $%d, $%d\n" ret_reg rreg);
        push_buffer (sprintf "L%d:\n" l2);
        reg_free rreg;
        ret_reg
      )
   | EAdd (e1, e2) ->
      (
        let ret_reg = reg_alloc () in
        let lreg = ex e1 in
        let rreg = ex e2 in
        push_buffer (sprintf "\tadd $%d, $%d, $%d\n" ret_reg lreg rreg);
        reg_free (ret_reg+1);
        ret_reg
      )
   | EShift (e1, e2) ->
      (
        let ret_reg = reg_alloc () in
        let lreg = ex e1 in
        let rreg = ex e2 in
        push_buffer (sprintf "\tshift $%d, $%d, $%d, 0\n" ret_reg lreg rreg);
        reg_free (ret_reg+1);
        ret_reg
      )
   | ESub (e1, e2) ->
      (
        let ret_reg = reg_alloc () in
        let lreg = ex e1 in
        let rreg = ex e2 in
        push_buffer (sprintf "\tsub $%d, $%d, $%d\n" ret_reg lreg rreg);
        reg_free (ret_reg+1);
        ret_reg
      )
   | ELe (e1, e2) ->
      (
        let ret_reg = reg_alloc () in
        let lreg = ex e1 in
        let rreg = ex e2 in
        let lnum = label_create () in
        push_buffer (sprintf "\tmov $%d, -1\n" ret_reg);
        push_buffer (sprintf "\tble $%d, $%d, L%d\n" lreg rreg lnum);
        push_buffer (sprintf "\tmov $%d, 0\n" ret_reg);
        push_buffer (sprintf "L%d:\n" lnum);
        reg_free lreg;
        ret_reg
      )
   | EEq (e1, e2) ->
      (
        let ret_reg = reg_alloc () in
        let lreg = ex e1 in
        let rreg = ex e2 in
        let lnum = label_create () in
        push_buffer (sprintf "\tmov $%d, -1\n" ret_reg);
        push_buffer (sprintf "\tbeq $%d, $%d, L%d\n" lreg rreg lnum);
        push_buffer (sprintf "\tmov $%d, 0\n" ret_reg);
        push_buffer (sprintf "L%d:\n" lnum);
        reg_free lreg;
        ret_reg
      )
   | ENeq (e1, e2) ->
      (
        let ret_reg = reg_alloc () in
        let lreg = ex e1 in
        let rreg = ex e2 in
        let lelse = label_create () in
        let lend = label_create () in
        push_buffer (sprintf "\tbeq $%d, $%d, L%d\n" lreg rreg lelse);
        push_buffer (sprintf "\tmov $%d, -1\n" ret_reg);
        push_buffer (sprintf "\tbr L%d\n" lend);
        push_buffer (sprintf "L%d:\n" lelse);
        push_buffer (sprintf "\tmov $%d, 0\n" ret_reg);
        push_buffer (sprintf "L%d:\n" lend);
        reg_free lreg;
        ret_reg
      )
   | EApp (Name fname, exlst) ->
      (
        let rec pfun i = function
          | [] -> ()
          | e::es ->
             let reg = ex e in
             if i != reg then
               push_buffer(sprintf "\tmov $%d, $%d\n" i reg);
             pfun (i+1) es in
        let used_num = !using_reg_num in
        for i = 1 to used_num do (*pushes*)
          push_buffer (sprintf "\tpush $%d\n" i)
        done;
        reg_free 1;
        pfun 1 exlst;
        push_buffer (sprintf "\tcall %s\n" fname);
        regs_alloc_num used_num;
        let ret_reg = reg_alloc () in
        if ret_reg != 1 then
          push_buffer (sprintf "\tmov $%d, $1\n" ret_reg);
        for i = used_num downto 1 do (*pops*)
          push_buffer (sprintf "\tpop $%d\n" i)
        done;
        ret_reg
      )
   | ESubst (dist, exp) ->
      let ret_reg =  ex exp in
      (match dist with
      | EVar (Name s) ->
         (match resolve_var s with
          | Reg i ->
             push_buffer (sprintf "\tmov $%d, $%d\n" i ret_reg);
             ret_reg
          | Mem offset ->
             push_buffer (sprintf "\tmov [$bp-%d], $%d\n" offset ret_reg);
             ret_reg
          | _ -> raise Unreachable
         )
      | EPtr e ->
         let r = ex e in
         push_buffer (sprintf "\tmov [$%d], $%d\n" r ret_reg);
         reg_free r;
         ret_reg
      | _ -> raise (EmitError "emit:substitution error")
      )

   | EAddr e -> getAddress e
   | EPtr e ->
      let addr_reg = ex e in
      let ret_reg = reg_alloc () in
      push_buffer (sprintf "\tmov $%d, [$%d]\n" ret_reg addr_reg);
      reg_free addr_reg;
      ret_reg
   | x ->
      fprintf stderr "In emit.ml\n";
      Print.pp_expr stderr x;
      fprintf stderr "\nThis expr can't be compiled yet.\n";
      raise (TODO "emit: ex"))
and getAddress l =
  match lv l with
  | Mem offset ->
     let ret_reg = reg_alloc () in
     push_buffer (sprintf "\tsub $%d, $bp, %d\n" ret_reg offset);
     ret_reg
  | Ptr x ->
     let rec go = function
       | Reg i ->
          let r = reg_alloc () in
          push_buffer (sprintf "\tmov $%d, $%d\n" r i);
          r
       | Mem offset ->
          let r = reg_alloc () in
          push_buffer (sprintf "\tmov $%d, [$bp-%d]\n" r offset);
          r
       | Ptr x ->
          let a = go x in
          let r = reg_alloc () in
          push_buffer (sprintf "\tmov $%d, [$%d]\n" r a);
          reg_free a;
          r in
     (go x)
  | _ -> raise (EmitError "can't get register's address\n")
and lv = function (*left-value -> storage place *)
  | EVar (Name s) -> resolve_var s
  | EPtr leftv -> Ptr (lv leftv)
  | EAddr leftv ->
     (match lv leftv with
      | Ptr x -> x
      | _ ->
         raise (EmitError (sprintf "This is not a left-value\n")))
  | x ->
     Print.pp_expr stderr x;
     raise (EmitError (sprintf "This is not a left-value\n"))
