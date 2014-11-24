open Syntax
open Printf
exception EmitError of string
exception Unreachable

let created_label_num = ref 0;;
let using_reg_num = ref 0;;
let max_reg_num = ref 0;;
let buffer_ref : string list ref = ref [];;
let sp_diff_ref = ref 0;;
type storageplace =
  | Reg of int (*register*)
  | Mem of int (*memory*)
  | Ptr of storageplace (*pointer*)
let env_ref : (ctype * string * storageplace) list ref = ref [];;

(* Access ref values*)
let push_buffer str =
  buffer_ref := str::!buffer_ref

let print_buffer oc name =
  fprintf oc ".global %s\n%s:\n" name name;
  let buf = List.rev !buffer_ref in
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

let push_vars flg lst =
  let len = List.length lst in
  if flg = 0 then (*引数を追加*)
    let l = List.map2
              (fun (ty, Name name) i -> (ty, name, Reg i))
              lst (range 1 len) in
    env_ref := l@(!env_ref)
  else (*ローカル変数を追加*)
    let idxlist = range (!sp_diff_ref+1) ((!sp_diff_ref)+len) in
    let l = List.map2
              (fun (ty, Name name) i -> (ty, name, Mem i))
              lst idxlist in
    env_ref := l@(!env_ref)

let pop_vars num =
  let rec drop xs n =
    match (n, xs) with
    | (0, _) -> xs
    | (_, []) -> []
    | (_, _::ys) -> drop ys (n - 1) in
  env_ref := drop (!env_ref) num


let rec main oc defs =
  let _ = List.map (fun x -> emit oc x) defs in
  ()
and emit oc = function
  | DFun(TInt, Name name, args, b, _) ->
     using_reg_num := List.length args;
     push_vars 0 args;
     bl b;
     pop_vars (List.length args);
     print_buffer oc name
  | _ -> raise (TODO "emit:emit")
and bl = function
  | Block (vars, stmts) ->
     let lvar_num = List.length vars in
     if lvar_num != 0 then
       (let reg = reg_alloc () in
        push_buffer (sprintf "\tmov $%d, %d\n" reg lvar_num);
        push_buffer (sprintf "\tsub $sp, $sp, $%d\n" reg);
        reg_free reg
       );
     push_vars 1 (List.map (fun sv ->let SVar(x,y) = sv in (x,y)) vars);
     sp_diff_ref := !sp_diff_ref + lvar_num;
     st stmts;
     sp_diff_ref := !sp_diff_ref - lvar_num;
     pop_vars lvar_num;
     if lvar_num != 0 then
       (let reg = reg_alloc () in
        push_buffer (sprintf "\tmov $%d, %d\n" reg lvar_num);
        push_buffer (sprintf "\tadd $sp, $sp, $%d\n" reg);
        reg_free reg
       )
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
     push_buffer (sprintf "\tbeq $0, $%d, L%d\n"
                          cond_reg
                          endlnum);
     reg_free cond_reg;
     bl b;
     push_buffer (sprintf "\tbr L%d\n" endlnum);
     push_buffer (sprintf "L%d:\n" endlnum)
  | SFor(init, cond, iter, b) ->
     let startlnum = label_create () in
     let endlnum = label_create () in
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
     bl b;
     (match iter with
      | Some itex ->
         let temp = ex itex in
         reg_free temp
      |  _ -> ());
     push_buffer (sprintf "\tbr L%d\n" startlnum);
     push_buffer (sprintf "L%d:\n" endlnum)
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
     if reg != 1 then
       push_buffer (sprintf "\tmov $1, $%d\n" reg);
     if !sp_diff_ref != 0 then
       (push_buffer (sprintf "\tmov $%d, %d\n" reg !sp_diff_ref);
        push_buffer (sprintf "\tadd $sp, $sp, $%d\n" reg));
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
   | EAdd (e1, e2) ->
      (
        let ret_reg = reg_alloc () in
        let lreg = ex e1 in
        let rreg = ex e2 in
        push_buffer (sprintf "\tadd $%d, $%d, $%d\n" ret_reg lreg rreg);
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
   | ESubst (l, exp) ->
      (
        let ret_reg = ex exp in
        match lv l with
        | Reg i ->
           push_buffer (sprintf "\tmov $%d, $%d\n" i ret_reg);
           ret_reg
        | Mem offset ->
           push_buffer (sprintf "\tmov [$bp-%d], $%d\n" offset ret_reg);
           ret_reg
        | _ ->
           let addr_reg = getAddress l in
           push_buffer (sprintf "\tmov [$%d], $%d\n" addr_reg ret_reg);
           reg_free addr_reg;
           ret_reg
      )
   | EAddr e -> getAddress e
   | EPtr e ->
      let addr_reg = ex e in
      let ret_reg = reg_alloc () in
      push_buffer (sprintf "\tmov $%d, [$%d]\n" ret_reg addr_reg);
      reg_free addr_reg;
      ret_reg
   | _ -> raise (TODO "emit: ex"))
and getAddress l =
  match lv l with
  | Mem offset ->
     let ret_reg = reg_alloc () in
     push_buffer (sprintf "\tsub $%d, $bp, %d\n" ret_reg offset);
     ret_reg
  | Ptr x ->
     let rec hoge = function
       | Reg i ->
          let ret_reg = reg_alloc () in
          push_buffer (sprintf "\tmov $%d, $%d\n" ret_reg i);
          ret_reg
       | Mem offset ->
          let r = reg_alloc () in
          push_buffer (sprintf "\tmov $%d, [$bp-%d]\n" r offset);
          r
       | Ptr x ->
          let a = hoge x in
          let r = reg_alloc () in
          push_buffer (sprintf "\tmov $%d, [$%d]\n" r a);
          reg_free a;
          r in
     (hoge x)
  | _ -> raise (EmitError "can't get register's address\n")
and lv = function (*left-value -> storage place *)
  | EVar (Name s) -> resolve_var s
  | EPtr leftv -> Ptr (lv leftv)
  | EAddr leftv ->
     (match lv leftv with
      | Ptr x -> x
      | _ ->
         raise (EmitError (sprintf "This is not a left-value\n")))
  | _ -> raise (EmitError (sprintf "This is not a left-value\n"))
