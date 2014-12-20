open Ctype
open Type
open Printf

exception EmitError of string

type storageplace =
  | Mem of int (*memory*)
  | Global of string

let register_list = Array.init 27 succ |> Array.to_list
let created_label_num = ref 0
let free_reg_stack = ref register_list
let buffer_ref : string list ref = ref []
let sp_offset_ref = ref 0
let con_stack : int list ref = ref []
let brk_stack : int list ref = ref []
let switch_counter = ref (-1)
let switch_stack = ref []
let switch_cases = ref []
let switch_defaults = ref []
let for_continue_flg_ref = ref 0
let fun_name_ref = ref ""
let env_ref : (string * (ctype * storageplace)) list ref = ref []

(* This is initialized in main *)
let senv_ref : (size * ((string * ctype) list)) list ref = ref []
let uenv_ref : (size * ((string * ctype) list)) list ref = ref []

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

let insert_prologue () =
  emit "mov r1, r0";
  emit "leave";
  emit "ret"

let insert_halt () =
  let trap s = if s = "\tret\n" then "\thalt\n" else s in
  buffer_ref := List.map trap !buffer_ref

let flush_buffer oc name =
  List.iter
    (fun s ->
     fprintf oc "%s" s)
    (List.rev !buffer_ref);
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
  try
    List.assoc name !env_ref
  with
  | Not_found -> raise (EmitError (sprintf "not found %s" name))

let resolve_struct s =
  List.nth !senv_ref s

let resolve_union u =
  List.nth !uenv_ref u

let rec sizeof = function
  | TInt | TShort | TLong | TUnsigned | TChar | TPtr _ -> 1
  | TArray (ty, sz) -> sz * (sizeof ty)
  | TStruct s_id -> fst (resolve_struct s_id)
  | TUnion u_id -> fst (resolve_union u_id)
  | TFun _ -> raise (EmitError "sizeof function")
  | TVoid -> raise (EmitError "sizeof void")

let sizeof_decl = function
  | Decl (NoLink,ty,_,_) ->
     sizeof ty * 4
  | Decl (Extern,_,_,_)
  | Decl (Static,_,_,_) ->
     0

let rec sizeof_block = function
  | SBlock (d, s) ->
     let s1 = List.fold_left (+) (0) (List.map sizeof_decl d) in
     let s2 = List.fold_left max (0) (List.map sizeof_block s) in
     s1 + s2
  | SWhile (_, s)
  | SDoWhile (s, _)
  | SFor (_, _, _, s)
  | SLabel (_, s)
  | SSwitch (_, s) ->
     sizeof_block s
  | SIfElse (_, s, t) ->
     max (sizeof_block s) (sizeof_block t)
  | _ ->
     0

let push_args args = (* add args in env *)
  let rec go i = function
    | [] -> ()
    | (Decl (_, ty, Name name, _))::xs ->
       env_ref := (name, (ty, Mem i))::!env_ref;
       go (i-4) xs in
  go (-4) args

let push_local_vars vars =
  let go = function
    | Decl (NoLink, ty, Name name, _) ->
       let sz = sizeof ty in
       sp_offset_ref := !sp_offset_ref + sz*4;
       stack_push env_ref (name, (ty, Mem !sp_offset_ref))
    | Decl (Extern, ty, Name name, _) ->
       stack_push env_ref (name, (ty, Global name))
    | Decl (Static, ty, Name name, _) ->
       let label_id = label_create () in
       let label = sprintf "L_%s_%d" name label_id in
       stack_push env_ref (name, (ty, Global label)) in
  List.iter go vars

let emit_global_var name init =
  let contents = ref [] in
  emit_raw "%s:\n" name;
  List.iter
    (fun e ->
     match e with
     | EConst (TInt, (VInt v)) -> emit ".int %d" v
     | EAddr (TPtr TInt, EConst (TArray (TInt, sz), VStr s)) ->
        contents := s :: !contents;
        emit ".int %s_contents_%d" name (List.length !contents)
     | _ -> raise (EmitError "global initializer must be constant")
    ) init;
  List.iteri
    (fun i c ->
     emit_raw "%s_contents_%d:\n" name (i + 1);
     List.iter (fun n -> emit ".int %d" n) c
    ) (List.rev !contents)

let rec ex ret_reg = function
  | EComma(_, ex1, ex2) ->
     ex ret_reg ex1;
     ex ret_reg ex2
  | EConst (_, v) ->
     (match v with
      | VInt i ->
         emit "mov r%d, %d" ret_reg i
      | VStr s ->
         raise (EmitError "logic flaw: EConst at Emitter.ex"))
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
     let t = TFun (TInt, [TInt;TInt]) in
     let t_ptr = TPtr t in
     (match op with
      | Mul ->
         ex ret_reg (ECall (t_ptr, EAddr (t, EVar(TInt, Name "__mul")), [e1;e2]))
      | Div ->
         ex ret_reg (ECall (t_ptr, EAddr (t, EVar(TInt, Name "__div")), [e1;e2]))
      | Mod ->
         ex ret_reg (ECall (t_ptr, EAddr (t, EVar(TInt, Name "__mod")), [e1;e2]))
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
         let ty_size = sizeof ty in
         ex ret_reg e1;
         let reg = reg_alloc () in
         (if ty_size = 1 then
           ex reg e2
         else
           let sz = EConst(TInt, VInt ty_size) in
           ex reg (EArith (TInt, Mul, e2, sz)));
         emit "shl r%d, r%d, 2" reg reg;
         emit "add r%d, r%d, r%d" ret_reg ret_reg reg;
         reg_free reg
      | _ ->
         failwith "EPAdd")
  | EPDiff (t1, e1, e2) ->
     raise (TODO "EPDiff")
  | ELog (_, op, e1, e2) ->
     (match op with
      | LogAnd ->
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
      | LogOr ->
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
         emit_lv_addr areg e;
         let reg = reg_alloc () in
         emit "mov r%d, [r%d]" ret_reg areg;
         if op = PostInc then
           emit "add r%d, r%d, 1" reg ret_reg
         else
           emit "sub r%d, r%d, 1" reg ret_reg;
         emit "mov [r%d], r%d" areg reg;
         reg_free areg;
         reg_free reg)
  | EPPost (ty, op, e) ->
     let areg = reg_alloc () in
     emit_lv_addr areg e;
     let reg = reg_alloc () in
     emit "mov r%d, [r%d]" ret_reg areg;
     if op = Inc then
       emit "add r%d, r%d, %d" reg ret_reg (4*sizeof ty)
     else
       emit "sub r%d, r%d, %d" reg ret_reg (4*sizeof ty);
     emit "mov [r%d], r%d" areg reg;
     reg_free areg;
     reg_free reg
  | ECall (_, f, exlst) ->
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
     let fun_reg = reg_alloc () in
     ex fun_reg f;
     reg_free fun_reg;
     emit "call r%d" fun_reg;
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
      | TArray _, _ | TFun _, _ ->
         raise (EmitError "logic flaw: EVar")
      | TStruct _, _ | TUnion _, _ ->
         raise (EmitError "EVar: struct as value is unsupported")
      | _ ->
         emit_lv_addr ret_reg (EVar (t, Name name));
         emit "mov r%d, [r%d]" ret_reg ret_reg)
  | EAssign (_, e1, e2) ->
     let reg = reg_alloc () in
     emit_lv_addr reg e1;
     ex ret_reg e2;
     emit "mov [r%d], r%d" reg ret_reg;
     reg_free reg
  | EAddr (_, e) ->
     emit_lv_addr ret_reg e
  | EPtr (_, e) ->
     ex ret_reg e;
     emit "mov r%d, [r%d]" ret_reg ret_reg
  | EDot (t, e, Name name) ->
     emit_lv_addr ret_reg (EDot (t, e, Name name));
     (match t with
      | TArray _ | TStruct _ | TUnion _ ->
         raise (EmitError "EDot")
      | _ ->
         emit "mov r%d, [r%d]" ret_reg ret_reg)
  | ECast (t1, t2, e) ->
     (match t1, t2 with
      | TStruct _, _ | _, TStruct _
      | TUnion  _, _ | _, TUnion  _
      | TArray  _, _ | _, TArray  _ ->
         raise (EmitError "ECast")
      | _ ->
         ex ret_reg e)

and emit_bin ret_reg op e1 e2 =
  ex ret_reg e1;
  let reg = reg_alloc () in
  ex reg e2;
  emit "%s r%d, r%d, r%d" op ret_reg ret_reg reg;
  reg_free reg

and emit_lv_addr ret_reg = function
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
           | [] -> failwith "edot"
           | (v, _)::_ when v=s -> i
           | (_, ty)::xs -> go (i+(sizeof ty)*4) s xs in
         let memlist = snd (resolve_struct s_id) in
         let mem_offset = go 0 mem memlist in
         emit_lv_addr ret_reg expr;
         emit "add r%d, r%d, %d" ret_reg ret_reg mem_offset
      | TUnion _ ->
         emit_lv_addr ret_reg expr
      | _ -> raise (EmitError "emit_lv_addr dot"))
  | EPtr (_, e) ->
     ex ret_reg e
  | EConst (_, VStr s) ->
     let ldata = label_create () in
     let ltext = label_create () in
     emit "br L%d" ltext;
     emit_label ldata;
     List.iter (fun i -> emit ".int %d" i) s;
     emit_label ltext;
     emit "mov r%d, L%d" ret_reg ldata
  | _ ->
     raise (EmitError "this expr is not lvalue")

let init_local_vars vars =
  let go (Decl (ln, _, Name nm, init)) =
    match resolve_var nm with
    | (_, Mem offset) ->
       let reg = reg_alloc () in
       List.iteri (fun i e ->
                   ex reg e;
                   emit "mov [rbp - %d], r%d" (offset - i * 4) reg
                  ) init;
       reg_free reg
    | (_, Global label) ->
       (match (ln, init) with
        | NoLink, _  -> failwith "init_local_vars"
        | Extern, [] ->
           ()                   (* ignore *)
        | Static, [] ->
           raise (EmitError "local static variable has no initializer")
        | Extern, xs ->
           raise (EmitError "local extern variable has initializer")
        | Static, xs ->
           emit_global_var label xs) in
  List.iter go vars

let rec st = function
  | SNil ->
     ()
  | SBlock (vars, stmts) ->
     let old_sp = !sp_offset_ref in
     let old_env = !env_ref in
     let old_senv = !senv_ref in
     push_local_vars vars;
     init_local_vars vars;
     List.iter st stmts;
     sp_offset_ref := old_sp;
     env_ref := old_env;
     senv_ref := old_senv;
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
     (match exp with
      | Some exp ->
         let reg = reg_alloc () in
         ex reg exp;
         if reg != 1 then
           emit "mov r1, r%d" reg;
         reg_free reg
      | None ->
         ());
     emit "leave";
     emit "ret";
  | SContinue ->
     let lbl = (List.hd !con_stack) in
     emit "br L%d" lbl;
     for_continue_flg_ref := 1
  | SBreak ->
     let lbl = (List.hd !brk_stack) in
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

let rec emitter oc = function
  | DefFun(Decl(ln, ty, Name name, _), args, b) ->
     stack_push env_ref (name, (ty, Global name));
     fun_name_ref := name;
     let free_regs = !free_reg_stack in
     let old_env = !env_ref in
     let old_senv = !senv_ref in
     push_args args;
     emit "enter %d" (sizeof_block b);
     st b;
     free_reg_stack := free_regs;
     env_ref := old_env;
     senv_ref := old_senv;
     insert_prologue ();
     if name = "main" then
       insert_halt ();
     (match ln with
      | NoLink
      | Extern ->
         fprintf oc ".global %s\n%s:\n" name name;
      | Static ->
         ());
     flush_buffer oc name
  | DefVar (Decl (ln, ty, Name name, init)) ->
     stack_push env_ref (name, (ty, Global name));
     (match (ln, init) with
      | NoLink, [] when not (is_funty ty) ->
         emit_raw ".global %s\n" name;
         emit_raw "%s:\n" name;
         emit ".int 0, %d\n" (sizeof ty)
      | NoLink, []
      | Extern, []
      | Static, [] ->
         ()                     (* ignore *)
      | NoLink, xs
      | Extern, xs ->
         emit_raw ".global %s\n" name;
         emit_global_var name xs
      | Static, xs ->
         emit_global_var name xs);
     flush_buffer oc name

(*emit main*)
let rec main oc defs =
  let go_struct l =
    let sz = List.fold_left (fun n (_, ty) -> n + sizeof ty) 0 l in
    senv_ref := !senv_ref @ [(sz, l)] in
  let go_union l =
    let sz = List.fold_left (fun n (_, ty) -> max n (sizeof ty)) 0 l in
    uenv_ref := !uenv_ref @ [(sz, l)] in
  List.iter go_struct !Typing.senv_ref;
  List.iter go_union  !Typing.uenv_ref;
  List.iter (emitter oc) defs
