open Ctype
open Type
open Printf
open Util

exception EmitError of string

type storageplace =
  | Mem of int (*memory*)
  | Global of string

let register_list = Array.init 27 succ |> Array.to_list
let free_reg_stack = ref register_list
let buffer_ref : string list ref = ref []
let env_ref : (string * (ctype * storageplace)) list ref = ref []
let fun_name_ref = ref ""
let sp_offset_ref = ref 0
let static_locals_ref = ref []
let for_continue_flg_ref = ref 0

(* label management *)
let created_label_num = ref 0
let con_stack : int list ref = ref []
let brk_stack : int list ref = ref []
let switch_counter = ref (-1)
let switch_stack = ref []
let switch_cases = ref []
let switch_defaults = ref []

let emit fmt =
  ksprintf (fun s -> push buffer_ref ("  "^s^"\n")) fmt
let emit_raw fmt =
  ksprintf (fun s -> push buffer_ref s) fmt
let emit_label num =
  push buffer_ref (sprintf "L%d:\n" num)

let insert_epilogue () =
  if !buffer_ref = [] || peek buffer_ref <> "  ret\n" then begin
    emit "mov r1, 0";
    emit "leave";
    emit "ret"
  end

let insert_halt () =
  let trap s = if s = "  ret\n" then "  halt\n" else s in
  buffer_ref := List.map trap !buffer_ref

let flush_buffer oc =
  List.iter
    (fun s ->
     fprintf oc "%s" s)
    (List.rev !buffer_ref);
  if !buffer_ref <> [] then fprintf oc "\n";
  buffer_ref := []

let reg_alloc () =
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

let reg_free_all () =
  free_reg_stack := register_list

let get_used_reg () =
  List.filter
    (fun x -> (List.mem x !free_reg_stack) = false)
    register_list

let label_create () =
  created_label_num := !created_label_num + 1;
  !created_label_num

let escape_label s =
  sprintf "L_label_%s_%s" !fun_name_ref s

let escape_case i =
  sprintf "L_case_%s_%d_%d" !fun_name_ref (peek switch_stack) i

let escape_default () =
  sprintf "L_default_%s_%d" !fun_name_ref (peek switch_stack)

let resolve_var name =
  try
    List.assoc name !env_ref
  with
  | Not_found -> raise (EmitError (sprintf "not found %s" name))

let sizeof_decl = function
  | Decl (NoLink,TFun _,_,_)
  | Decl (Extern,_,_,_)
  | Decl (Static,_,_,_) ->
     0
  | Decl (NoLink,ty,_,_) ->
     sizeof ty * 4

let rec sizeof_block = function
  | SBlock (d, s) ->
     let s1 = sum_of (List.map sizeof_decl d) in
     let s2 = max_of (List.map sizeof_block s) in
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
    | Decl (NoLink, ((TFun _) as ty), Name name, _)
    | Decl (Extern, ty, Name name, _) ->
       push env_ref (name, (ty, Global name))
    | Decl (NoLink, ty, Name name, _) ->
       let sz = sizeof ty in
       sp_offset_ref := !sp_offset_ref + sz*4;
       push env_ref (name, (ty, Mem !sp_offset_ref))
    | Decl (Static, ty, Name name, _) ->
       let label_id = label_create () in
       let label = sprintf "L_%s_%d" name label_id in
       push env_ref (name, (ty, Global label)) in
  List.iter go vars

let create_defualt_init ty =
  Util.rep (EConst (TInt, (VInt 0))) (sizeof ty)

let emit_global_var name init =
  let contents = ref [] in
  emit_raw "%s:\n" name;
  let rec go = function
    | EConst (TInt, (VInt v)) ->
       emit ".int %d" v
    | EAddr (TPtr TInt, EConst (TArray (TInt, _), VStr s)) ->
       contents := s :: !contents;
       emit ".int %s_contents_%d" name (List.length !contents)
    | EAddr (TPtr _, EVar (_, Name v)) when List.mem_assoc v !env_ref ->
       emit ".int %s" v
    | ECast (_, _, e) -> go e
    | _ -> raise (EmitError "global initializer must be constant") in
  List.iter go init;
  List.iteri
    (fun i c ->
     emit_raw "%s_contents_%d:\n" name (i + 1);
     List.iter (fun n -> emit ".int %d" n) c)
    (List.rev !contents)

let emit_native_call ret_reg func arg1 arg2 =
  let used_reg = List.filter (fun x -> x != ret_reg) (get_used_reg ()) in
  List.iter (emit "push r%d") used_reg;
  emit "push r%d" arg2;
  emit "push r%d" arg1;
  let fun_reg = reg_alloc () in
  emit "mov r%d, %s" fun_reg func;
  emit "call r%d" fun_reg;
  reg_free_all ();
  reg_use ret_reg;
  if ret_reg != 1 then
    emit "mov r%d, r1" ret_reg;
  emit "add rsp, rsp, 8";
  List.iter (fun i -> reg_use i; emit "pop r%d" i) (List.rev used_reg)

let mem_access (reg, disp) =
  let reg = if reg = 31 then "rbp" else sprintf "r%d" reg in
  if disp > 0 then sprintf "[%s + %d]" reg disp else
  if disp < 0 then sprintf "[%s - %d]" reg (-disp) else
  sprintf "[%s]" reg

let rec ex ret_reg = function
  | ENil -> ()
  | EComma(_, ex1, ex2) ->
     ex ret_reg ex1;
     ex ret_reg ex2
  | EConst (_, v) ->
     begin match v with
     | VInt i ->
        emit "mov r%d, %d" ret_reg i
     | VFloat f ->
        emit "mov r%d, %F" ret_reg f
     | VStr _ ->
        raise (EmitError "logic flaw: EConst at Emitter.ex")
     end
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
     begin match op with
     | Mul | Div | Mod ->
        let log2 n =
          let rec go n acc =
            if n <= 1 then acc else go (n / 2) (acc + 1) in
          go n 0 in
        begin match e1, e2 with
        | _, EConst (_, VInt x) when x land (x - 1) = 0 ->
           ex ret_reg e1;
           begin match op, ty with
           | Mul, _ | Div, _ when x = 1 ->
              ()
           | Mul, _ when x = 0 ->
              emit "mov r%d, 0" ret_reg
           | Mod, _ when x = 1 ->
              emit "mov r%d, 0" ret_reg
           | Mul, _ ->
              emit "shl r%d, r%d, %d" ret_reg ret_reg (log2 x)
           | Div, TUInt ->
              emit "shr r%d, r%d, %d" ret_reg ret_reg (log2 x)
           | Div, _ ->
              let reg = reg_alloc () in
              emit "sar r%d, r%d, 31" reg ret_reg;
              emit "and r%d, r%d, %d" reg reg (x - 1);
              emit "add r%d, r%d, r%d" ret_reg ret_reg reg;
              emit "sar r%d, r%d, %d" ret_reg ret_reg (log2 x);
              reg_free reg
           | Mod, TUInt ->
              emit "and r%d, r%d, %d" ret_reg ret_reg (x - 1)
           | Mod, _ ->
              let reg = reg_alloc () in
              if x = 2 then
                emit "shr r%d, r%d, 31" reg ret_reg
              else begin
                emit "sar r%d, r%d, 31" reg ret_reg;
                emit "shr r%d, r%d, %d" reg reg (32 - log2 x)
              end;
              emit "add r%d, r%d, r%d" ret_reg ret_reg reg;
              emit "and r%d, r%d, %d" ret_reg ret_reg (x - 1);
              emit "sub r%d, r%d, r%d" ret_reg ret_reg reg;
              reg_free reg
           | _ ->
              assert false
           end
        | EConst (_, VInt x), _ when x land (x - 1) = 0 && op = Mul ->
           ex ret_reg (EArith (ty, op, e2, e1))
        | _ ->
           let fun_name =
             match op, ty with
             | Div, TUInt -> "__unsigned_div"
             | Mod, TUInt -> "__unsigned_mod"
             | Mul, _ -> "__mul"
             | Div, _ -> "__signed_div"
             | Mod, _ -> "__signed_mod"
             | _ -> assert false in
           ex ret_reg e1;
           let reg = reg_alloc () in
           ex reg e2;
           emit_native_call ret_reg fun_name ret_reg reg;
           reg_free reg
        end
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
        emit_bin ret_reg op e1 e2
     end
  | EFArith (ty, op, e1, e2) ->
     let op = match op with
       | Add    -> "fadd"
       | Sub    -> "fsub"
       | Mul    -> "fmul"
       | Div    -> "fdiv"
       | _ -> assert false in
     emit_bin ret_reg op e1 e2
  | ERel (_, op, e1, e2) ->
     let op = match op with
       | Le -> "cmple"
       | Lt -> "cmplt"
       | Ge -> "cmpge"
       | Gt -> "cmpgt" in
     emit_bin ret_reg op e1 e2
  | EURel (_, op, e1, e2) ->
     let op = match op with
       | Le -> "cmple"
       | Lt -> "cmplt"
       | Ge -> "cmpge"
       | Gt -> "cmpgt" in
     ex ret_reg e1;
     let reg  = reg_alloc () in
     ex reg e2;
     let sreg = reg_alloc () in
     emit "ldh r%d, r0, 0x8000" sreg;
     emit "xor r%d, r%d, r%d" ret_reg ret_reg sreg;
     emit "xor r%d, r%d, r%d" reg reg sreg;
     emit "%s r%d, r%d, r%d" op ret_reg ret_reg reg;
     reg_free reg;
     reg_free sreg
  | EFRel (_, op, e1, e2) ->
     let op = match op with
       | Le -> "fcmple"
       | Lt -> "fcmplt"
       | Ge -> "fcmpge"
       | Gt -> "fcmpgt" in
     emit_bin ret_reg op e1 e2
  | EEq (_, op, e1, e2) ->
     let op = match op with
       | Eq -> "cmpeq"
       | Ne -> "cmpne" in
     emit_bin ret_reg op e1 e2
  | EFEq (_, op, e1, e2) ->
     let op = match op with
       | Eq -> "fcmpeq"
       | Ne -> "fcmpne" in
     emit_bin ret_reg op e1 e2
  | EPAdd (ty, e1, e2) ->
     begin match ty with
     | TPtr ty ->
        ex ret_reg e1;
        let reg = reg_alloc () in
        ex reg e2;
        ex reg (EArith (TInt, Mul, ENil, EConst (TInt, VInt (4 * sizeof ty))));
        emit "add r%d, r%d, r%d" ret_reg ret_reg reg;
        reg_free reg
     | _ ->
        failwith "EPAdd"
     end
  | EPDiff (_, e1, e2) ->
     begin match (Typing.typeof e1, Typing.typeof e2) with
     | (TPtr t1, TPtr t2) when t1 = t2 ->
        ex ret_reg e1;
        let reg = reg_alloc () in
        ex reg e2;
        emit "sub r%d, r%d, r%d" ret_reg ret_reg reg;
        emit "mov r%d, %d" reg (4 * sizeof t1);
        emit_native_call ret_reg "__signed_div" ret_reg reg;
        reg_free reg
     | _ ->
        failwith "EPDiff"
     end
  | ELog (_, op, e1, e2) ->
     begin match op with
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
        emit_label l2
     end
  | EUnary (_, op, e) ->
     begin match op with
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
        let mem = emit_lv_addr areg e in
        let reg = reg_alloc () in
        emit "mov r%d, %s" ret_reg (mem_access mem);
        if op = PostInc then
          emit "add r%d, r%d, 1" reg ret_reg
        else
          emit "sub r%d, r%d, 1" reg ret_reg;
        emit "mov %s, r%d" (mem_access mem) reg;
        reg_free areg;
        reg_free reg
     end
  | EFUnary (_, op, e) ->
     begin match op with
     | Plus ->
        ex ret_reg e
     | Minus ->
        ex ret_reg e;
       emit "xor r%d, r%d, 0x80000000" ret_reg ret_reg
     | _ ->
        raise (EmitError "FUnary")
     end
  | EPPost (TPtr ty, op, e) ->
     let areg = reg_alloc () in
     let mem = emit_lv_addr areg e in
     let reg = reg_alloc () in
     emit "mov r%d, %s" ret_reg (mem_access mem);
     if op = Inc then
       emit "add r%d, r%d, %d" reg ret_reg (4 * sizeof ty)
     else
       emit "sub r%d, r%d, %d" reg ret_reg (4 * sizeof ty);
     emit "mov %s, r%d" (mem_access mem) reg;
     reg_free areg;
     reg_free reg
  | EPPost _ ->
     raise (EmitError "EPPost: not pointer")
  | ECall (_, EAddr(_, EVar(_, Name "__asm")),
           [ECast(_, _, EAddr (_, EConst(_, VStr asm)))]) ->
     let slist = List.map (Char.chr >> String.make 1) asm in
     emit_raw "%s" (String.concat "" (Util.take (List.length slist - 1) slist))
  | ECall (_, f, exlst) ->
     let used_reg = List.filter (fun x -> x != ret_reg) (get_used_reg ()) in
     let arg_list = List.map
                      (fun e ->
                       let reg = reg_alloc () in
                       ex reg e;
                       reg) exlst in
     let fun_reg = reg_alloc () in
     ex fun_reg f;
     (* save registers *)
     List.iter (fun reg -> emit "push r%d" reg) used_reg;
     (* push arguments *)
     List.iter (fun reg -> emit "push r%d" reg) (List.rev arg_list);
     emit "call r%d" fun_reg;
     reg_free_all ();
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
  | EVar (ty, Name name) as expr ->
     begin match resolve_var name with
     | TArray _, _ | TFun _, _ ->
        raise (EmitError "logic flaw: EVar")
     | TStruct _, _ | TUnion _, _ ->
        raise (EmitError "EVar: struct as value is unsupported")
     | _ ->
        let mem = emit_lv_addr ret_reg expr in
        emit "mov r%d, %s" ret_reg (mem_access mem)
     end
  | EAssign (ty, op, e1, e2) ->
     let reg = reg_alloc () in
     let mem = emit_lv_addr reg e1 in
     ex ret_reg e2;
     begin match op with
     | None ->
        ()
     | Some op ->
        let tmp_reg = reg_alloc () in
        emit "mov r%d, %s" tmp_reg (mem_access mem);
        begin match op, ty with
        | Add, TPtr ty ->
           if sizeof ty != 0 then begin
             let size_reg = reg_alloc () in
             emit "mov r%d, %d" size_reg (sizeof ty);
             emit_native_call ret_reg "__mul" ret_reg size_reg;
             reg_free size_reg
           end;
           emit "shl r%d, r%d, 2" ret_reg ret_reg;
           emit "add r%d, r%d, r%d" ret_reg tmp_reg ret_reg
        | Add, _ ->
           emit "add r%d, r%d, r%d" ret_reg tmp_reg ret_reg
        | Sub, _ ->
           emit "sub r%d, r%d, r%d" ret_reg tmp_reg ret_reg
        | LShift, _ ->
           emit "shl r%d, r%d, r%d" ret_reg tmp_reg ret_reg
        | RShift, _ ->
           emit "shr r%d, r%d, r%d" ret_reg tmp_reg ret_reg
        | BitAnd, _ ->
           emit "and r%d, r%d, r%d" ret_reg tmp_reg ret_reg
        | BitXor, _ ->
           emit "xor r%d, r%d, r%d" ret_reg tmp_reg ret_reg
        | BitOr, _ ->
           emit "or r%d, r%d, r%d" ret_reg tmp_reg ret_reg
        | Mul, _ ->
           emit_native_call ret_reg "__mul" tmp_reg ret_reg
        | Div, _ ->
           if ty = TUInt then
             emit_native_call ret_reg "__unsigned_div" tmp_reg ret_reg
           else
             emit_native_call ret_reg "__signed_div" tmp_reg ret_reg
        | Mod, _ ->
           if ty = TUInt then
             emit_native_call ret_reg "__unsigned_mod" tmp_reg ret_reg
           else
             emit_native_call ret_reg "__signed_mod" tmp_reg ret_reg
        end;
        reg_free tmp_reg
     end;
     emit "mov %s, r%d" (mem_access mem) ret_reg;
     reg_free reg
  | EFAssign (ty, op, e1, e2) ->
     let reg = reg_alloc () in
     let mem = emit_lv_addr reg e1 in
     ex ret_reg e2;
     begin match op with
     | None ->
        ()
     | Some op ->
        let tmp_reg = reg_alloc () in
        emit "mov r%d, %s" tmp_reg (mem_access mem);
        let fop =
          begin match op with
          | Add -> "fadd"
          | Sub -> "fsub"
          | Mul -> "fmul"
          | Div -> "fdiv"
          | _   -> raise (EmitError "EFAssign")
          end in
        emit "%s r%d, r%d, r%d" fop ret_reg tmp_reg ret_reg;
        reg_free tmp_reg
     end;
     emit "mov %s, r%d" (mem_access mem) ret_reg;
     reg_free reg
  | EAddr (_, e) ->
     let (reg, disp) = emit_lv_addr ret_reg e in
     if not (ret_reg = reg && disp = 0) then
       let reg = if reg = 31 then "rbp" else sprintf "r%d" reg in
       emit "add r%d, %s, %d" ret_reg reg disp
  | EPtr (_, e) ->
     ex ret_reg e;
     emit "mov r%d, [r%d]" ret_reg ret_reg
  | EDot (ty, e, Name name) as expr ->
     let mem = emit_lv_addr ret_reg expr in
     begin match ty with
     | TArray _ | TStruct _ | TUnion _ ->
        raise (EmitError "EDot")
     | _ ->
        emit "mov r%d, %s" ret_reg (mem_access mem)
     end
  | ECast (t1, t2, e) ->
     begin match t1, t2 with
     | TStruct _, _ | _, TStruct _
     | TUnion  _, _ | _, TUnion  _
     | TArray  _, _ | _, TArray  _ ->
        raise (EmitError "ECast")
     | _, _ when t1 = t2 ->
       ex ret_reg e
     | TFloat, TInt ->
       ex ret_reg e;
       emit "itof r%d, r%d" ret_reg ret_reg
     | TInt, TFloat ->
       ex ret_reg e;
       let flg = reg_alloc () in
       emit "sar r%d, r%d, 31" flg ret_reg;    (* flg=ret<0?-1:0 *)
       emit "shl r%d, r%d, 1"  ret_reg ret_reg;
       emit "shr r%d, r%d, 1"  ret_reg ret_reg;(* fabs(ret_reg)*)
       emit "floor r%d, r%d" ret_reg ret_reg;
       emit "ftoi  r%d, r%d" ret_reg ret_reg;
       (* (x^flg)-flg equals (flg==-1?-x:x) *)
       emit "xor r%d, r%d, r%d" ret_reg ret_reg flg;
       emit "sub r%d, r%d, r%d" ret_reg ret_reg flg;
       reg_free flg
     | TFloat, TUInt
     | TUInt, TFloat ->
        raise (EmitError "ECast: float <-> unsigned is unsupported")
     | TFloat, _
     | _, TFloat ->
        raise (EmitError "ECast: float")
     | _ ->
        ex ret_reg e
     end

and emit_bin ret_reg op e1 e2 =
  ex ret_reg e1;
  let reg = reg_alloc () in
  ex reg e2;
  emit "%s r%d, r%d, r%d" op ret_reg ret_reg reg;
  reg_free reg

and emit_lv_addr ret_reg = function
  | EVar (_, Name name) ->
     begin match resolve_var name with
     | (_, Mem offset) ->
        (31, (-offset))
     | (_, Global label) ->
        emit "mov r%d, %s" ret_reg label;
        (ret_reg, 0)
     end
  | EDot (_, expr, Name mem) ->
     begin match Typing.typeof expr with
     | TStruct s_id ->
        let rec go i s = function
          | [] -> failwith "edot"
          | (v, _)::_ when v=s -> i
          | (_, ty)::xs -> go (i+(sizeof ty)*4) s xs in
        let memlist = List.nth !struct_env s_id in
        let mem_offset = go 0 mem memlist in
        let (reg, disp) = emit_lv_addr ret_reg expr in
        (reg, disp + mem_offset)
     | TUnion _ ->
        emit_lv_addr ret_reg expr
     | _ -> raise (EmitError "emit_lv_addr dot")
     end
  | EPtr (_, e) ->
     ex ret_reg e;
     (ret_reg, 0)
  | EConst (_, VStr s) ->
     let label = sprintf "L%d" (label_create ()) in
     let t = List.map (fun i -> EConst (TInt, VInt i)) s in
     push static_locals_ref (label, t);
     emit "mov r%d, %s" ret_reg label;
     (ret_reg, 0)
  | _ ->
     raise (EmitError "this expr is not lvalue")

let init_local_vars vars =
  let go (Decl (ln, ty, Name nm, init)) =
    match resolve_var nm with
    | (_, Mem offset) ->
       let reg = reg_alloc () in
       List.iteri (fun i e ->
                   ex reg e;
                   emit "mov [rbp - %d], r%d" (offset - i * 4) reg
                  ) init;
       reg_free reg
    | (_, Global label) ->
       match (ln, init) with
       | Static, [] ->
          push static_locals_ref (label, create_defualt_init ty)
       | Static, xs ->
          push static_locals_ref (label, xs)
       | Extern, [] | NoLink, [] ->
          ()                   (* ignore *)
       | Extern, _ | NoLink, _ ->
          raise (EmitError "local extern variable has initializer") in
  List.iter go vars

let rec st = function
  | SNil ->
     ()
  | SBlock (vars, stmts) ->
     let old_sp = !sp_offset_ref in
     let old_env = !env_ref in
     push_local_vars vars;
     init_local_vars vars;
     List.iter st stmts;
     sp_offset_ref := old_sp;
     env_ref := old_env;
  | SWhile (cond, b) ->
     let beginlabel = label_create () in
     let endlabel = label_create () in
     push con_stack beginlabel;
     push brk_stack endlabel;
     emit_label beginlabel;
     let cond_reg = reg_alloc () in
     ex cond_reg cond;
     emit "bz r%d, L%d" cond_reg endlabel;
     reg_free cond_reg;
     st b;
     emit "br L%d" beginlabel;
     emit_label endlabel;
     pop con_stack;
     pop brk_stack
  | SDoWhile (b, cond) ->
     let beginlabel = label_create () in
     let condlabel = label_create () in
     let endlabel = label_create () in
     push con_stack condlabel;
     push brk_stack endlabel;
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
     pop con_stack;
     pop brk_stack
  | SFor(init, cond, iter, b) ->
     let startlnum = label_create () in
     let iterlnum = label_create () in
     let endlnum = label_create () in
     push con_stack iterlnum;
     push brk_stack endlnum;
     begin match init with
     | Some iex ->
        let temp = reg_alloc () in
        ex temp iex;
        reg_free temp
     | _ -> ()
     end;
     emit_label startlnum;
     begin match cond with
     | Some cex ->
        let cond_reg = reg_alloc () in
        ex cond_reg cex;
        emit "bz r%d, L%d" cond_reg endlnum;
        reg_free cond_reg
     | _ -> ()
     end;
     let continue_flg = !for_continue_flg_ref in
     for_continue_flg_ref := 0;
     st b;
     if !for_continue_flg_ref = 1 then
       emit_label iterlnum;
     for_continue_flg_ref := continue_flg;
     begin match iter with
     | Some itex ->
        let temp = reg_alloc () in
        ex temp itex;
        reg_free temp
     |  _ -> ()
     end;
     emit "br L%d" startlnum;
     emit_label endlnum;
     pop con_stack;
     pop brk_stack
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
     begin match exp with
     | Some exp ->
        let reg = reg_alloc () in
        ex reg exp;
        if reg != 1 then
          emit "mov r1, r%d" reg;
        reg_free reg
     | None ->
        ()
     end;
     emit "leave";
     emit "ret";
  | SContinue ->
     let lbl = peek con_stack in
     emit "br L%d" lbl;
     for_continue_flg_ref := 1
  | SBreak ->
     let lbl = peek brk_stack in
     emit "br L%d" lbl
  | SLabel (label, s) ->
     emit_raw "%s:\n" (escape_label label);
     st s
  | SGoto label ->
     emit "br %s" (escape_label label)
  | SCase i ->
     switch_cases := (i :: peek switch_cases) :: List.tl !switch_cases;
     emit_raw "%s:\n" (escape_case i)
  | SDefault ->
     peek switch_defaults := true;
     emit_raw "%s:\n" (escape_default ())
  | SSwitch (e,s) ->
     switch_counter := !switch_counter + 1;
     switch_stack := !switch_counter :: !switch_stack;
     switch_cases := [] :: !switch_cases;
     switch_defaults := ref false :: !switch_defaults;
     let l1 = label_create () in
     let l2 = label_create () in
     emit "br L%d" l1;
     push brk_stack l2;
     st s;
     pop brk_stack;
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
       (peek switch_cases);
     reg_free lreg;
     reg_free rreg;
     if !(peek switch_defaults) then
       emit "br %s" (escape_default ());
     emit_label l2;
     switch_defaults := List.tl !switch_defaults;
     switch_cases := List.tl !switch_cases;
     switch_stack := List.tl !switch_stack
  | SExpr exp ->
     let temp = reg_alloc () in
     ex temp exp;
     reg_free temp

let emitter oc = function
  | DefFun(Decl(ln, ty, Name name, _), args, b) ->
     push env_ref (name, (ty, Global name));
     fun_name_ref := name;
     static_locals_ref := [];
     begin match ln with
     | NoLink
     | Extern ->
        emit_raw ".global %s\n" name;
        emit_raw "%s:\n" name
     | Static ->
        emit_raw "%s:\n" name
     end;
     let free_regs = !free_reg_stack in
     let old_env = !env_ref in
     push_args args;
     begin match b with
     | SBlock ([], [SExpr (ECall (_, EAddr (_, EVar(_, Name "__asm")),
                    [ECast (_, _, EAddr (_, EConst(_, VStr asm)))]))]) ->
        ()
     | _ ->
        emit "enter %d" (sizeof_block b)
     end;
     st b;
     free_reg_stack := free_regs;
     env_ref := old_env;
     insert_epilogue ();
     if name = "main" then
       insert_halt ();
     List.iter (fun (name,e) ->
       emit_global_var name e
     ) (List.rev !static_locals_ref);
     flush_buffer oc
  | DefVar (Decl (ln, ty, Name name, init)) ->
     push env_ref (name, (ty, Global name));
     begin match (ln, init) with
     | NoLink, [] when not (is_funty ty) ->
        emit_raw ".global %s\n" name;
        emit_global_var name (create_defualt_init ty)
     | Static, [] when not (is_funty ty) ->
        emit_global_var name (create_defualt_init ty)
     | NoLink, []
     | Extern, []
     | Static, [] ->
        ()                     (* ignore *)
     | NoLink, xs
     | Extern, xs ->
        emit_raw ".global %s\n" name;
        emit_global_var name xs
     | Static, xs ->
        emit_global_var name xs
     end;
     flush_buffer oc

let main oc defs =
  List.iter (emitter oc) defs
