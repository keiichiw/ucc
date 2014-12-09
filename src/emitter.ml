open Type
open Printf
exception EmitError of string
exception TODO of string
exception Unreachable
type storageplace =
  | Reg of int (*register*)
  | Mem of int (*memory*)
let register_list = [1;2;3;4;5;6;7;8;9;10;11];;
let created_label_num = ref 0;;
let free_reg_stack = ref [1;2;3;4;5;6;7;8;9;10;11];;
let buffer_ref : string list ref = ref [];;
let sp_offset_ref = ref 0;;
let con_stack : int list ref = ref [];;
let brk_stack : int list ref = ref [];;
let switch_counter = ref (-1);;
let switch_stack = ref [];;
let switch_cases = ref [];;
let switch_defaults = ref [];;
let sp_move_stack : int list ref = ref [];;
let for_continue_flg_ref = ref 0;;
let fun_name_ref = ref "";;
let env_ref : (string * (ctype * storageplace)) list ref = ref [];;
let struct_env_ref : (string * (int * ((string * ctype) list))) list ref = ref [];;

(* Access ref values*)
let stack_push stack i =
  stack := (i::!stack)
let stack_append stack l =
  stack := (l@(!stack))
let stack_pop stack =
  stack := (List.tl !stack)

let push_buffer str =
  stack_push buffer_ref str
let append_buffer sl =
  stack_append buffer_ref sl

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
  match !free_reg_stack with
  | [] ->
     raise (EmitError "register starvation!!");
  | x::xs ->
     free_reg_stack := xs;
     x
let reg_use i =
  if List.mem i !free_reg_stack then
    free_reg_stack := List.filter (fun x->x!=i) !free_reg_stack
  else
    raise (EmitError (sprintf "Register $%d is not free!!" i))

let reg_free i =
  if List.mem i !free_reg_stack then
    raise (EmitError (sprintf "Register $%d is already free!!" i))
  else
    free_reg_stack := i::!free_reg_stack

let reg_free_all _ =
  free_reg_stack := register_list

let get_used_reg _ =
  List.filter
    (fun x -> (List.mem x !free_reg_stack) = false)
    register_list

let label_create _ =
  created_label_num := !created_label_num + 1;
  !created_label_num

let escape_label s =
  sprintf "L_label_%s_%s" !fun_name_ref s

let escape_case i =
  sprintf "L_case_%s_%d_%d" !fun_name_ref (List.hd !switch_stack) i

let escape_default () =
  sprintf "L_default_%s_%d" !fun_name_ref (List.hd !switch_stack)

let resolve_var name =
  List.assoc name !env_ref
let resolve_struct name =
  List.assoc name !struct_env_ref

let rec size_of = function
  | TInt
  | TPtr _ -> 1
  | TArray (ty, sz) -> sz * (size_of ty)
  | TStruct (Some (Name name),_) ->
     let (sz, _) = resolve_struct name in
     sz
  | _ -> raise (TODO "size of struct")
let size_of_dvar = function
  | DVar (ty,_,_) -> size_of ty
  | DStruct _ -> 0
let get_dvar_name = function
  | DVar (_,Name n, _) -> n
  | _ -> raise Unreachable
let get_dvar_type = function
  | DVar (t,_, _) -> t
  | _ -> raise Unreachable
let push_args args = (* add args in env *)
  let rec go i = function
    | [] -> ()
    | (DVar (ty, Name name, _))::xs ->
       reg_use i;
       env_ref := (name, (ty, Reg i))::!env_ref;
       go (i+1) xs
    | _ -> raise (EmitError "array can't be an argument of function.") in
  go 1 args
let push_global_var = function (* add global var in env *)
  | DStruct (Name name, dvars) ->
     let f = (fun dv -> (get_dvar_name dv, get_dvar_type dv)) in
     let vars = List.map f dvars in
     let sz = List.fold_left (fun n (_,x) -> n+(size_of x)) 0 vars in
     stack_push struct_env_ref (name, (sz, vars));
     ()
  | _ -> raise (TODO "global variables isn\'t supported yet")

let push_local_vars vars =
  let go = function
    | DVar (ty, Name name, _) ->
       let sz = size_of ty in
       sp_offset_ref := !sp_offset_ref + sz;
       stack_push env_ref (name, (ty, Mem !sp_offset_ref))
    | DStruct (Name name, dvars) ->
       let f d = (get_dvar_name d, get_dvar_type d) in
       let vlist = List.map f dvars in
       let sz = List.fold_left (fun n (_,t) -> n+(size_of t)) 0 vlist in
       stack_push struct_env_ref (name, (sz, vlist)) in
  List.iter go vars
(*emit main*)
let rec main oc defs =
  List.iter (emitter oc) defs
and emitter oc = function
  | DefFun(ty, Name name, args, b, _) ->
     fun_name_ref := name;
     let free_regs = !free_reg_stack in
     let old_env = !env_ref in
     let old_senv = !struct_env_ref in
     push_args args;
     bl b;
     free_reg_stack := free_regs;
     env_ref := old_env;
     struct_env_ref := old_senv;
     print_buffer oc name
  | DefVar v ->
     push_global_var v
and bl = function
  | Block (vars, stmts) ->
     let old_sp = !sp_offset_ref in
     let old_env = !env_ref in
     let old_senv = !struct_env_ref in
     push_local_vars vars;
     let sp_move = !sp_offset_ref - old_sp in
     stack_push sp_move_stack sp_move;
     if sp_move != 0 then
       push_buffer (sprintf "\tsub $sp, $sp, %d\n" sp_move);
     init_local_vars vars;
     List.iter st stmts;
     sp_offset_ref := old_sp;
     env_ref := old_env;
     struct_env_ref := old_senv;
     stack_pop sp_move_stack;
     if sp_move != 0 then
       push_buffer (sprintf "\tadd $sp, $sp, %d\n" sp_move)
and init_local_vars vars =
  let go = function
    | DVar (_, Name nm, Some x) ->
       (match resolve_var nm with
        | (_, Mem offset) ->
           let reg = reg_alloc () in
           ex reg x;
           push_buffer (sprintf "\tmov [$bp-%d], $%d\n" offset reg);
           reg_free reg
        |_ -> raise Unreachable)
    | _ -> () in
  List.iter go vars
and st = function
  | SNil -> ()
  | SBlock (x,y) ->
     bl (Block (x,y))
  | SWhile (cond, b) ->
     let beginlabel = label_create () in
     let endlabel = label_create () in
     stack_push con_stack beginlabel;
     stack_push brk_stack endlabel;
     push_buffer (sprintf "L%d:\n" beginlabel);
     let cond_reg = reg_alloc () in
     ex cond_reg cond;
     push_buffer (sprintf "\tbeq $0, $%d, L%d\n"
                          cond_reg
                          endlabel);
     reg_free cond_reg;
     st b;
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
     st b;
     if !for_continue_flg_ref = 1 then
       push_buffer (sprintf "L%d:\n" condlabel);
     for_continue_flg_ref := continue_flg;
     let cond_reg = reg_alloc () in
     ex cond_reg cond;
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
         let temp = reg_alloc () in
         ex temp iex;
         reg_free temp
      | _ -> ());
     push_buffer (sprintf "L%d:\n" startlnum);
     (match cond with
      | Some cex ->
         let cond_reg = reg_alloc () in
         ex cond_reg cex;
         push_buffer (sprintf "\tbeq $0, $%d, L%d\n" cond_reg endlnum);
         reg_free cond_reg
      | _ -> ());
     let continue_flg = !for_continue_flg_ref in
     for_continue_flg_ref := 0;
     st b;
     if !for_continue_flg_ref = 1 then
       push_buffer (sprintf "L%d:\n" iterlnum);
     for_continue_flg_ref := continue_flg;
     (match iter with
      | Some itex ->
         let temp = reg_alloc () in
         ex temp itex;
         reg_free temp
      |  _ -> ());
     push_buffer (sprintf "\tbr L%d\n" startlnum);
     push_buffer (sprintf "L%d:\n" endlnum);
     stack_pop con_stack;
     stack_pop brk_stack
  | SIfElse (cond, b1, b2) ->
     let cond_reg = reg_alloc () in
     ex cond_reg cond;
     let lnum = label_create () in
     let endlnum = label_create () in
     push_buffer (sprintf "\tbeq $0, $%d, L%d\n"
                          cond_reg
                          lnum);
     reg_free cond_reg;
     st b1;
     push_buffer (sprintf "\tbr L%d\n" endlnum);
     push_buffer (sprintf "L%d:\n" lnum);
     st b2;
     push_buffer (sprintf "L%d:\n" endlnum)
  | SReturn exp ->
     let reg = reg_alloc () in
     ex reg exp;
     if !sp_offset_ref != 0 then
       push_buffer (sprintf "\tadd $sp, $sp, %d\n" !sp_offset_ref);
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
  | SLabel (label, s) ->
     let lbl = escape_label label in
     push_buffer (sprintf "%s:\n" lbl);
     st s
  | SGoto label ->
     let lbl = escape_label label in
     push_buffer (sprintf "\tbr %s\n" lbl)
  | SCase e ->
     (match e with
      | EConst (_, VInt i) ->
         switch_cases := (i :: List.hd !switch_cases) :: List.tl !switch_cases;
         push_buffer (sprintf "%s:\n" (escape_case i))
      | _ ->
         raise (EmitError "non-constant value at case clause"))
  | SDefault ->
     (List.hd !switch_defaults) := true;
     push_buffer (sprintf "%s:\n" (escape_default ()))
  | SSwitch (e,s) ->
     switch_counter := !switch_counter + 1;
     switch_stack := !switch_counter :: !switch_stack;
     switch_cases := [] :: !switch_cases;
     switch_defaults := ref false :: !switch_defaults;
     let l1 = label_create () in
     let l2 = label_create () in
     push_buffer (sprintf "\tbr L%d\n" l1);
     stack_push brk_stack l2;
     st s;
     stack_pop brk_stack;
     push_buffer (sprintf "\tbr L%d\n" l2);
     (* dispatcher *)
     push_buffer (sprintf "L%d:\n" l1);
     let lreg = reg_alloc () in
     ex lreg e;
     let rreg = reg_alloc () in
     List.iter
       (fun i ->
        push_buffer (sprintf "\tmov $%d, %d\n" rreg i);
        push_buffer (sprintf "\tbeq $%d, $%d, %s\n" lreg rreg (escape_case i)))
       (List.hd !switch_cases);
     reg_free lreg;
     reg_free rreg;
     if !(List.hd !switch_defaults) then
       push_buffer (sprintf "\tbr %s\n" (escape_default ()));
     push_buffer (sprintf "L%d:\n" l2);
     switch_defaults := List.tl !switch_defaults;
     switch_cases := List.tl !switch_cases;
     switch_stack := List.tl !switch_stack
  | SExpr exp ->
     let temp = reg_alloc () in
     ex temp exp;
     reg_free temp
and ex ret_reg = function
  | EComma(_, ex1, ex2) ->
     ex ret_reg ex1;
     ex ret_reg ex2
  | EConst (_, VInt i) ->
     push_buffer (sprintf "\tmov $%d, %d\n" ret_reg i)
  | ECond (_, c, t, e) ->
     let lelse = label_create () in
     let lend = label_create () in
     ex ret_reg c;
     push_buffer (sprintf "\tbeq $%d, $0, L%d\n" ret_reg lelse);
     ex ret_reg t;
     push_buffer (sprintf "\tbr L%d\n" lend);
     push_buffer (sprintf "L%d:\n" lelse);
     ex ret_reg e;
     push_buffer (sprintf "L%d:\n" lend)
  | EAnd (_, e1, e2) ->
     let l1 = label_create () in
     let l2 = label_create () in
     ex ret_reg e1;
     push_buffer (sprintf "\tbeq $%d, $0, L%d\n" ret_reg l1);
     ex ret_reg e2;
     push_buffer (sprintf "\tbr L%d\n" l2);
     push_buffer (sprintf "L%d:\n" l1);
     push_buffer (sprintf "\tmov $%d, $0\n" ret_reg);
     push_buffer (sprintf "L%d:\n" l2)
  | EOr (_, e1, e2) ->
     let l1 = label_create () in
     let l2 = label_create () in
     ex ret_reg e1;
     push_buffer (sprintf "\tbeq $%d, $0, L%d\n" ret_reg l1);
     push_buffer (sprintf "\tbr L%d\n" l2);
     push_buffer (sprintf "L%d:\n" l1);
     ex ret_reg e2;
     push_buffer (sprintf "L%d:\n" l2)
  | EAdd (_, e1, e2) ->
     ex ret_reg e1;
     let rreg = reg_alloc () in
     ex rreg e2;
     push_buffer (sprintf "\tadd $%d, $%d, $%d\n" ret_reg ret_reg rreg);
     reg_free rreg
  | ESub (_, e1, e2) ->
     ex ret_reg e1;
     let rreg = reg_alloc () in
     ex rreg e2;
     push_buffer (sprintf "\tsub $%d, $%d, $%d\n" ret_reg ret_reg rreg);
     reg_free rreg
  | EShift (_, e1, e2) ->
     ex ret_reg e1;
     let rreg = reg_alloc () in
     ex rreg e2;
     push_buffer (sprintf "\tshift $%d, $%d, $%d\n" ret_reg ret_reg rreg);
     reg_free rreg
  | ELe (_, e1, e2) ->
     let lnum = label_create () in
     let lreg = reg_alloc () in
     ex lreg e1;
     let rreg = reg_alloc () in
     ex rreg e2;
     push_buffer (sprintf "\tmov $%d, 1\n" ret_reg);
     push_buffer (sprintf "\tble $%d, $%d, L%d\n" lreg rreg lnum);
     push_buffer (sprintf "\tmov $%d, 0\n" ret_reg);
     push_buffer (sprintf "L%d:\n" lnum);
     reg_free lreg;
     reg_free rreg
  | EEq (_, e1, e2) ->
     let lnum = label_create () in
     let lreg = reg_alloc () in
     ex lreg e1;
     let rreg = reg_alloc () in
     ex rreg e2;
     push_buffer (sprintf "\tmov $%d, 1\n" ret_reg);
     push_buffer (sprintf "\tbeq $%d, $%d, L%d\n" lreg rreg lnum);
     push_buffer (sprintf "\tmov $%d, 0\n" ret_reg);
     push_buffer (sprintf "L%d:\n" lnum);
     reg_free lreg;
     reg_free rreg
  | ENeq (_, e1, e2) ->
     let lelse = label_create () in
     let lend = label_create () in
     let lreg = reg_alloc () in
     ex lreg e1;
     let rreg = reg_alloc () in
     ex rreg e2;
     push_buffer (sprintf "\tbeq $%d, $%d, L%d\n" lreg rreg lelse);
     push_buffer (sprintf "\tmov $%d, 1\n" ret_reg);
     push_buffer (sprintf "\tbr L%d\n" lend);
     push_buffer (sprintf "L%d:\n" lelse);
     push_buffer (sprintf "\tmov $%d, 0\n" ret_reg);
     push_buffer (sprintf "L%d:\n" lend);
     reg_free lreg;
     reg_free rreg
  | EApp (_, Name fname, exlst) ->
     let used_reg = List.filter (fun x -> x != ret_reg) (get_used_reg ()) in
     List.iter (fun i -> push_buffer (sprintf "\tpush $%d\n" i))
               used_reg;
     reg_free_all ();
     let _ = List.fold_left (fun i e -> reg_use i;ex i e; i+1) 1 exlst in
     reg_free_all ();
     push_buffer (sprintf "\tcall %s\n" fname);
     reg_use ret_reg;
     if ret_reg != 1 then
       push_buffer (sprintf "\tmov $%d, $1\n" ret_reg);
     List.iter (fun i ->
                reg_use i;
                push_buffer (sprintf "\tpop $%d\n" i))
               (List.rev used_reg)
  | EArray (_, e1, idx) ->
     lv_addr ret_reg e1;
     let ireg = reg_alloc () in
     ex ireg idx;
     push_buffer (sprintf "\tadd $%d, $%d, $%d\n" ret_reg ret_reg ireg);
     push_buffer (sprintf "\tmov $%d, [$%d]\n" ret_reg ret_reg);
     reg_free ireg
  | EVar (t, Name name) ->
     (match resolve_var name with
      | (_, Reg i) ->
         push_buffer (sprintf "\tmov $%d, $%d\n" ret_reg i)
      | _ ->
         (*型で場合分けする？*)
         lv_addr ret_reg (EVar (t, Name name));
         push_buffer (sprintf "\tmov $%d, [$%d]\n" ret_reg ret_reg))
  | ESubst (_, e1, e2) -> (*引数に代入できない*)
     let reg = reg_alloc () in
     lv_addr reg e1;
     ex ret_reg e2;
     push_buffer (sprintf "\tmov [$%d], $%d\n" reg ret_reg);
     reg_free reg
  | EAddr (_, e) ->
     lv_addr ret_reg e
  | EPtr (_, e) ->
     ex ret_reg e;
     push_buffer (sprintf "\tmov $%d, [$%d]\n" ret_reg ret_reg)
  | EDot (t, e, Name name) ->
     lv_addr ret_reg (EDot (t, e, Name name));
     push_buffer (sprintf "\tmov $%d, [$%d]\n" ret_reg ret_reg)
and lv_addr ret_reg = function
  | EVar (_, Name name) ->
     (match resolve_var name with
      | (_, Mem offset) ->
         push_buffer (sprintf "\tsub $%d, $bp, %d\n" ret_reg offset)
      | _ ->
         raise (EmitError "args \'s address"))
  | EArray (_, lv, idx) ->
     (match (lv, idx) with
      | (EVar(_, Name nm), EConst(TInt, VInt(i))) ->
         (match resolve_var nm with
          | (_, Mem offset) ->
             push_buffer
               (sprintf"\tsub $%d, $bp, %d\n" ret_reg (offset-i))
          | _ -> raise Unreachable)
      | _ ->
         let lv_reg = reg_alloc () in
         lv_addr lv_reg lv;
         let idx_reg = reg_alloc () in
         ex idx_reg idx;
         push_buffer (sprintf"\tadd $%d, $%d, $%d\n" ret_reg lv_reg idx_reg);
         reg_free lv_reg;
         reg_free idx_reg)
  | EDot (_, expr, Name mem) ->
     (match Typing.typeof expr with
      | TStruct (Some (Name sname), _) ->
         let rec go i s = function
           | [] -> raise Unreachable
           | (v, _)::_ when v=s -> i
           | (_, ty)::xs -> go (i+(size_of ty)) s xs in
         let (_, memlist) = resolve_struct sname in
         let mem_offset = go 0 mem memlist in
         lv_addr ret_reg expr;
         push_buffer (sprintf "\tadd $%d, $%d, %d\n" ret_reg ret_reg mem_offset)
      | _ -> raise (EmitError "lv_addr dot"))
  | EPtr (_, e) -> ex ret_reg e
  | e ->
     (match Typing.typeof e with
      | TPtr _
      | TArray _ -> ex ret_reg e
      | TStruct _ -> raise (TODO "lv struct")
      | _ -> raise (EmitError "this expr is not lvalue"))
