open Type
open Printf
exception EmitError of string
exception TODO of string
exception Unreachable of string
type storageplace =
  | Mem of int (*memory*)
  | Global of string
let register_list = Array.init 27 succ |> Array.to_list;;
let created_label_num = ref 0;;
let free_reg_stack = ref register_list;;
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

(* This is initialized in main *)
let struct_env_ref :
      (struct_id * (size * ((string * ctype) list))) list ref= ref [];;

(* Access ref values*)
let stack_push stack i =
  stack := (i::!stack)
let stack_append stack l =
  stack := (l@(!stack))
let stack_pop stack =
  stack := (List.tl !stack)

let emit fmt =
  ksprintf (fun s -> stack_push buffer_ref ("\t"^s^"\n")) fmt
let emit_raw fmt =
  ksprintf (fun s -> stack_push buffer_ref s) fmt
let emit_label num =
  stack_push buffer_ref (sprintf "L%d:\n" num)

let print_buffer oc name =
  fprintf oc ".global %s\n%s:\n" name name;
  let buf' = List.rev !buffer_ref in
  (* don't insert halt directly. the way of handling halt can be changed in the future *)
  let buf = buf' @ [ "\tmov r1, r0\n"; "\tleave\n"; "\tret\n" ] in
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
    raise (EmitError (sprintf "Register r%d is not free!!" i))

let reg_free i =
  if List.mem i !free_reg_stack then
    raise (EmitError (sprintf "Register r%d is already free!!" i))
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
let resolve_struct s =
  let rec go s_id = function
    | [] -> raise (EmitError (sprintf "struct %d not found" s_id))
    | (s, x)::_ when s_id = s -> x
    | _::xs -> go s_id xs in
  go s !struct_env_ref

let rec size_of = function
  | TInt
  | TUnsigned
  | TPtr _ -> 1
  | TArray (ty, sz) -> sz * (size_of ty)
  | TStruct s_id ->
     fst (resolve_struct s_id)
let size_of_dvar = function
  | DVar (ty,_,_) -> size_of ty
let get_dvar_name = function
  | DVar (_,Name n, _) -> n
let get_dvar_type = function
  | DVar (t,_, _) -> t
let push_args args = (* add args in env *)
  let rec go i = function
    | [] -> ()
    | (DVar (ty, Name name, _))::xs ->
       env_ref := (name, (ty, Mem i))::!env_ref;
       go (i-4) xs in
  go (-4) args
let push_global_var oc = function (* add global var in env *)
  | DVar (ty, Name name, []) ->
     let label = sprintf "global_%s" name in
     stack_push env_ref (name, (ty, Global label));
     fprintf oc "%s:\n" label;
     fprintf oc "\t.int 0, %d\n" (size_of ty)
  | DVar (ty, Name name, xs) ->
     let label = sprintf "global_%s" name in
     stack_push env_ref (name, (ty, Global label));
     fprintf oc "%s:\n" label;
     List.iter (fun e ->
        match e with
        | EConst (TInt, (VInt v)) -> fprintf oc "\t.int %d\n" v
        | _ -> raise (EmitError "global initializer must be constant")
     ) xs


let push_local_vars vars =
  let go = function
    | DVar (ty, Name name, _) ->
       let sz = size_of ty in
       sp_offset_ref := !sp_offset_ref + sz*4;
       stack_push env_ref (name, (ty, Mem (!sp_offset_ref+4))) in
  List.iter go vars
(*emit main*)
let rec main oc defs =
  let go (i, l) =
    let sz = List.fold_left (fun num (_, ty) -> num + (size_of ty)) 0 l in
    let tuple = (i, (sz, l)) in
    struct_env_ref := tuple::!struct_env_ref in
  List.iter go !Typing.senv_ref;
  List.iter (emitter oc) defs
and emitter oc = function
  | DefFun(ty, Name name, args, b) ->
     fun_name_ref := name;
     let free_regs = !free_reg_stack in
     let old_env = !env_ref in
     let old_senv = !struct_env_ref in
     push_args args;
     emit "enter 0";
     st b;
     free_reg_stack := free_regs;
     env_ref := old_env;
     struct_env_ref := old_senv;
     print_buffer oc name
  | DefVar v ->
     push_global_var oc v
and init_local_vars vars =
  let go = function
    | DVar (_, Name nm, xs) ->
       (match resolve_var nm with
        | (_, Mem offset) ->
           let reg = reg_alloc () in
           List.iteri (fun i e ->
              ex reg e;
              emit "mov [rbp - %d], r%d" (offset - i * 4) reg
           ) xs;
           reg_free reg
        |_ -> raise (Unreachable "init_local_var")) in
  List.iter go vars
and st = function
  | SNil ->
     ()
  | SBlock (vars, stmts) ->
     let old_sp = !sp_offset_ref in
     let old_env = !env_ref in
     let old_senv = !struct_env_ref in
     push_local_vars vars;
     let sp_move = !sp_offset_ref - old_sp in
     stack_push sp_move_stack sp_move;
     if sp_move != 0 then
       emit "sub rsp, rsp, %d" sp_move;
     init_local_vars vars;
     List.iter st stmts;
     sp_offset_ref := old_sp;
     env_ref := old_env;
     struct_env_ref := old_senv;
     stack_pop sp_move_stack;
     if sp_move != 0 then
       emit "add rsp, rsp, %d" sp_move
  | SWhile (cond, b) ->
     let beginlabel = label_create () in
     let endlabel = label_create () in
     stack_push con_stack beginlabel;
     stack_push brk_stack endlabel;
     emit_label beginlabel;
     let cond_reg = reg_alloc () in
     ex cond_reg cond;
     emit "bz r%d, L%d" cond_reg endlabel;
     reg_free cond_reg;
     st b;
     emit "br L%d" beginlabel;
     emit_label endlabel;
     stack_pop con_stack;
     stack_pop brk_stack
  | SDoWhile (b, cond) ->
     let beginlabel = label_create () in
     let condlabel = label_create () in
     let endlabel = label_create () in
     stack_push con_stack condlabel;
     stack_push brk_stack endlabel;
     emit_label beginlabel;
     let continue_flg = !for_continue_flg_ref in
     for_continue_flg_ref := 0;
     st b;
     if !for_continue_flg_ref = 1 then
       emit_label condlabel;
     for_continue_flg_ref := continue_flg;
     let cond_reg = reg_alloc () in
     ex cond_reg cond;
     emit "bz r%d, L%d" cond_reg endlabel;
     emit "br L%d" beginlabel;
     emit_label endlabel;
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
     emit_label startlnum;
     (match cond with
      | Some cex ->
         let cond_reg = reg_alloc () in
         ex cond_reg cex;
         emit "bz r%d, L%d" cond_reg endlnum;
         reg_free cond_reg
      | _ -> ());
     let continue_flg = !for_continue_flg_ref in
     for_continue_flg_ref := 0;
     st b;
     if !for_continue_flg_ref = 1 then
       emit_label iterlnum;
     for_continue_flg_ref := continue_flg;
     (match iter with
      | Some itex ->
         let temp = reg_alloc () in
         ex temp itex;
         reg_free temp
      |  _ -> ());
     emit "br L%d" startlnum;
     emit_label endlnum;
     stack_pop con_stack;
     stack_pop brk_stack
  | SIfElse (cond, b1, b2) ->
     let cond_reg = reg_alloc () in
     ex cond_reg cond;
     let lnum = label_create () in
     let endlnum = label_create () in
     emit "bz r%d, L%d" cond_reg lnum;
     reg_free cond_reg;
     st b1;
     emit "br L%d" endlnum;
     emit_label lnum;
     st b2;
     emit_label endlnum
  | SReturn exp ->
     let reg = reg_alloc () in
     ex reg exp;
     if !sp_offset_ref != 0 then
       emit "add rsp, rsp, %d" !sp_offset_ref;
     if reg != 1 then
       emit "mov r1, r%d" reg;
     emit "leave";
     emit "ret";
     reg_free reg
  | SContinue ->
     let lbl = (List.hd !con_stack) in
     emit "br L%d" lbl;
     for_continue_flg_ref := 1
  | SBreak ->
     let lbl = (List.hd !brk_stack) in
     let sp_d = (List.hd !sp_move_stack) in
     emit "add rsp, rsp, %d" sp_d;
     emit "br L%d" lbl
  | SLabel (label, s) ->
     emit_raw "%s:\n" (escape_label label);
     st s
  | SGoto label ->
     emit "br %s" (escape_label label)
  | SCase i ->
     switch_cases := (i :: List.hd !switch_cases) :: List.tl !switch_cases;
     emit_raw "%s:\n" (escape_case i)
  | SDefault ->
     (List.hd !switch_defaults) := true;
     emit_raw "%s:\n" (escape_default ())
  | SSwitch (e,s) ->
     switch_counter := !switch_counter + 1;
     switch_stack := !switch_counter :: !switch_stack;
     switch_cases := [] :: !switch_cases;
     switch_defaults := ref false :: !switch_defaults;
     let l1 = label_create () in
     let l2 = label_create () in
     emit "br L%d" l1;
     stack_push brk_stack l2;
     st s;
     stack_pop brk_stack;
     emit "br L%d" l2;
     (* dispatcher *)
     emit_label l1;
     let lreg = reg_alloc () in
     ex lreg e;
     let rreg = reg_alloc () in
     List.iter
       (fun i ->
        emit "mov r%d, %d" rreg i;
        emit "beq r%d, r%d, %s" lreg rreg (escape_case i))
       (List.hd !switch_cases);
     reg_free lreg;
     reg_free rreg;
     if !(List.hd !switch_defaults) then
       emit "br %s" (escape_default ());
     emit_label l2;
     switch_defaults := List.tl !switch_defaults;
     switch_cases := List.tl !switch_cases;
     switch_stack := List.tl !switch_stack
  | SExpr exp ->
     let temp = reg_alloc () in
     ex temp exp;
     reg_free temp
and emit_bin ret_reg op e1 e2 =
  ex ret_reg e1;
  let reg = reg_alloc () in
  ex reg e2;
  emit "%s r%d, r%d, r%d" op ret_reg ret_reg reg;
  reg_free reg
and ex ret_reg = function
  | EComma(_, ex1, ex2) ->
     ex ret_reg ex1;
     ex ret_reg ex2
  | EConst (_, VInt i) ->
     emit "mov r%d, %d" ret_reg i
  | ECond (_, c, t, e) ->
     let lelse = label_create () in
     let lend = label_create () in
     ex ret_reg c;
     emit "bz r%d, L%d" ret_reg lelse;
     ex ret_reg t;
     emit "br L%d" lend;
     emit_label lelse;
     ex ret_reg e;
     emit_label lend
  | EArith (ty, op, e1, e2) ->
     (match op with
      | Mul ->
         ex ret_reg (ECall (ty, EVar(TInt, Name "__mul"),[e1;e2]))
      | Div ->
         ex ret_reg (ECall (ty, EVar(TInt, Name "__div"),[e1;e2]))
      | Mod ->
         ex ret_reg (ECall (ty, EVar(TInt, Name "__mod"),[e1;e2]))
      | _ ->
         let op = match op with
           | Add    -> "add"
           | Sub    -> "sub"
           | LShift -> "shl"
           | RShift -> "shr"
           | BitAnd -> "and"
           | BitOr  -> "or"
           | BitXor -> "xor"
           | _ -> assert false in
         emit_bin ret_reg op e1 e2)
  | ERel (ty, op, e1, e2) ->
     let op = match op with
       | Le -> "cmple"
       | Lt -> "cmplt"
       | Ge -> "cmpge"
       | Gt -> "cmpgt" in
     emit_bin ret_reg op e1 e2
  | EEq (ty, op, e1, e2) ->
     let op = match op with
       | Eq -> "cmpeq"
       | Ne -> "cmpne" in
     emit_bin ret_reg op e1 e2
  | EPAdd (t1, e1, e2) ->
     (match (Typing.typeof e1, Typing.typeof e2) with
      | (TPtr ty, i) when Typing.is_integral i ->
         let ty_size = size_of ty in
         ex ret_reg e1;
         let reg = reg_alloc () in
         (if ty_size = 1 then
           ex reg e2
         else
           let sz = EConst(TInt, VInt ty_size) in
           ex reg (ECall (ty, EVar(TInt, Name "__mul"), [e2;sz])));
         emit "shl r%d, r%d, 2" reg reg;
         emit "add r%d, r%d, r%d" ret_reg ret_reg reg;
         reg_free reg
      | _ ->
         raise (Unreachable "EPAdd"))
  | EPDiff (t1, e1, e2) ->
     raise (TODO "EPDiff")
  | ELog (_, op, e1, e2) ->
     (match op with
      | And ->
         let l1 = label_create () in
         let l2 = label_create () in
         ex ret_reg e1;
         emit "bz r%d, L%d" ret_reg l1;
         ex ret_reg e2;
         emit "bz r%d, L%d" ret_reg l1;
         emit "mov r%d, 1" ret_reg;
         emit "br L%d" l2;
         emit_label l1;
         emit "mov r%d, 0" ret_reg;
         emit_label l2
      | Or ->
         let l1 = label_create () in
         let l2 = label_create () in
         ex ret_reg e1;
         emit "bnz r%d, L%d" ret_reg l1;
         ex ret_reg e2;
         emit "bnz r%d, L%d" ret_reg l1;
         emit "br L%d" l2;
         emit_label l1;
         emit "mov r%d, 1" ret_reg;
         emit_label l2)
  | EUnary (ty, op, e) ->
     (match op with
      | Plus ->
         ex ret_reg e
      | Minus ->
         ex ret_reg e;
         emit "neg r%d, r%d" ret_reg ret_reg
      | BitNot ->
         ex ret_reg e;
         emit "not r%d, r%d" ret_reg ret_reg
      | LogNot ->
         ex ret_reg e;
         emit "cmpeq r%d, r%d, 0" ret_reg ret_reg
      | PostInc
      | PostDec ->
         let areg = reg_alloc () in
         lv_addr areg e;
         let reg = reg_alloc () in
         emit "mov r%d, [r%d]" ret_reg areg;
         if op = PostInc then
           emit "add r%d, r%d, 1" reg ret_reg
         else
           emit "sub r%d, r%d, 1" reg ret_reg;
         emit "mov [r%d], r%d" areg reg;
         reg_free areg;
         reg_free reg)
  | ECall (_, f, exlst) ->
     let fname =
       (match f with
        | EVar (_, Name nm) -> nm
        | _ -> raise (EmitError "ECall: fname")) in
     let used_reg = List.filter (fun x -> x != ret_reg) (get_used_reg ()) in
     let arg_list = List.map
                      (fun e ->
                       let reg = reg_alloc () in
                       ex reg e;
                       reg) exlst in
     (* save registers *)
     List.iter (fun reg ->
                reg_free reg;
                emit "push r%d" reg)
               used_reg;
     (* push arguments *)
     List.iter (fun reg ->
                reg_free reg;
                emit "push r%d" reg)
               (List.rev arg_list);
     reg_free_all ();
     emit "call %s" fname;
     reg_use ret_reg;
     if ret_reg != 1 then
       emit "mov r%d, r1" ret_reg;
     (* clean arguments *)
     emit "add rsp, rsp, %d" (List.length arg_list*4);
     (* restore registers *)
     List.iter (fun i ->
                reg_use i;
                emit "pop r%d" i)
               (List.rev used_reg)
  | EVar (t, Name name) ->
     (match resolve_var name with
      | (TArray  _, _)
      | (TStruct _, _) ->
         lv_addr ret_reg (EVar (t, Name name));
      | _ ->
         lv_addr ret_reg (EVar (t, Name name));
         emit "mov r%d, [r%d]" ret_reg ret_reg)
  | EAssign (_, e1, e2) ->
     let reg = reg_alloc () in
     lv_addr reg e1;
     ex ret_reg e2;
     emit "mov [r%d], r%d" reg ret_reg;
     reg_free reg
  | EAddr (_, e) ->
     lv_addr ret_reg e
  | EPtr (_, e) ->
     ex ret_reg e;
     emit "mov r%d, [r%d]" ret_reg ret_reg
  | EDot (t, e, Name name) ->
     lv_addr ret_reg (EDot (t, e, Name name));
     (match t with
      | TArray _ | TStruct _ -> ()
      | _ ->
         emit "mov r%d, [r%d]" ret_reg ret_reg)
  | EArray (t, e1, e2) ->
     lv_addr ret_reg (EArray (t, e1, e2));
     (match t with
      | TArray _ | TStruct _ -> ()
      | _ ->
         emit "mov r%d, [r%d]" ret_reg ret_reg)
  | ECast (t1, t2, e) ->
     (match (t1, t2) with
      | (TStruct _, _)
      | (_, TStruct _)
      | (TArray _, _)
      | (_, TArray _) ->
         raise (EmitError "ECast")
      | _ ->
         ex ret_reg e)
and lv_addr ret_reg = function
  | EVar (_, Name name) ->
     (match resolve_var name with
      | (_, Mem offset) ->
         emit "sub r%d, rbp, %d" ret_reg offset
      | (_, Global label) ->
         emit "mov r%d, %s" ret_reg label)
  | EDot (_, expr, Name mem) ->
     (match Typing.typeof expr with
      | TStruct s_id ->
         let rec go i s = function
           | [] -> raise (Unreachable "edot")
           | (v, _)::_ when v=s -> i
           | (_, ty)::xs -> go (i+(size_of ty)*4) s xs in
         let (_, memlist) = resolve_struct s_id in
         let mem_offset = go 0 mem memlist in
         lv_addr ret_reg expr;
         emit "add r%d, r%d, %d" ret_reg ret_reg mem_offset
      | _ -> raise (EmitError "lv_addr dot"))
  | EPtr (_, e) -> ex ret_reg e
  | EArray (ty, e1, e2) ->
     ex ret_reg (EPAdd(Type.TPtr ty, e1, e2))
  | e ->
     (match Typing.typeof e with
      | TPtr _ -> ex ret_reg e
      | TStruct _ -> raise (TODO "lv struct")
      | _ -> raise (EmitError "this expr is not lvalue"))
