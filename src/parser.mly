%{
open Syntax
open Ctype
open Parser_helper
open Util

exception ParserError of string

type declarator =
  | DeclPtr of declarator
  | DeclIdent of name
  | DeclArray of declarator * size
  | DeclFun of declarator * (decl list)

let struct_table : (string * (int * int)) list ref = ref []
let union_table  : (string * (int * int)) list ref = ref []

let scope_stack = ref []

let get_namety = function
  | Decl (_, ty, name, _) -> (name, ty)

let add_struct_decl name_opt decl =
  let id = List.length !struct_env in
  if is_some name_opt then begin
    let depth = List.length !scope_stack in
    push struct_table (from_some name_opt, (id, depth));
    push rev_table_struct (id, from_some name_opt)
  end;
  struct_env := !struct_env @ [List.map get_namety decl];
  TStruct id

let add_union_decl name_opt decl =
  let id = List.length !union_env in
  if is_some name_opt then begin
    let depth = List.length !scope_stack in
    push union_table (from_some name_opt, (id, depth));
    push rev_table_struct (id, from_some name_opt)
  end;
  union_env := !union_env @ [List.map get_namety decl];
  TUnion id

let get_ty = function
  | Decl (_, TArray (ty, sz), _, _) ->
     TPtr ty
  | Decl (_, ty, _, _) ->
     ty

let make_decl ln ty (decl, exp) =
  let name = ref "" in
  let rec go k = function
    | DeclIdent n ->
       name := n;
       k ty
    | DeclPtr d ->
       go (fun x -> TPtr (k x)) d
    | DeclArray (d, sz) ->
       go (fun x -> TArray (k x, sz)) d
    | DeclFun (d, dvs) ->
       go (fun x -> TFun (k x, List.map get_ty dvs)) d in
  let ty = go id decl in
  typedef_env := List.filter (((<>) !name) << fst) !typedef_env;
  enum_env := List.filter (((<>) !name) << fst) !enum_env;
  Decl (ln, ty, !name, exp)

let make_decls ln ty decls =
  if decls = [] then begin
    let rec go target = function
      | (name, (id, depth)) :: _ when id = target ->
         (depth = List.length !scope_stack, name)
      | _ :: l ->
         go target l
      | [] ->
         assert false in
    match ty with
    | TStruct s_id ->
       let (flag, name) = go s_id !struct_table in
       if not flag then ignore (add_struct_decl (Some name) []) else ()
    | TUnion u_id ->
       let (flag, name) = go u_id !union_table in
       if not flag then ignore (add_union_decl (Some name) []) else ()
    | _ ->
       ()
  end;
  List.map (make_decl ln ty) decls

let rec get_params = function
  | DeclFun (_, dvs) ->
     let go = function
       | Decl(ln, TArray (ty, _), nm, e) ->
          Decl(ln, TPtr ty, nm, e)
       | x -> x in
     List.map go dvs
  | DeclPtr d -> get_params d
  | _ -> raise (ParserError "get_params")

let make_type ty decl =
  match make_decl NoLink ty (decl, None) with
   | Decl (_, ty, "", None) ->
      ty
   | _ ->
      raise (ParserError "make_type")

let lookup_structty name =
  try
    TStruct (fst (List.assoc name !struct_table))
  with Not_found ->
    add_struct_decl (Some name) []

let lookup_unionty name =
  try
    TUnion (fst (List.assoc name !union_table))
  with Not_found ->
    add_union_decl (Some name) []

let insert_struct_decl name decl =
  try
    let (id, depth) = List.assoc name !struct_table in
    if depth = List.length !scope_stack then begin
      struct_env := list_set !struct_env id (List.map get_namety decl);
      TStruct id
    end else
      add_struct_decl (Some name) decl
  with Not_found ->
    add_struct_decl (Some name) decl

let insert_union_decl name decl =
  try
    let (id, depth) = List.assoc name !union_table in
    if depth = List.length !scope_stack then begin
      union_env := list_set !union_env id (List.map get_namety decl);
      TUnion id
    end else
      add_union_decl (Some name) decl
  with Not_found ->
    add_union_decl (Some name) decl

let make_structty decl = function
  | Some name ->
     insert_struct_decl name decl
  | None ->
     add_struct_decl None decl

let make_unionty decl = function
  | Some name ->
     insert_union_decl name decl
  | None ->
     add_union_decl None decl

let make_enumty enums =
  let go num = function
    | (enum, Some cnst) ->
       enum_def enum cnst;
       cnst+1
    | (enum, None) ->
       enum_def enum num;
       num+1 in
  ignore (List.fold_left go 0 enums)

let scope_enter () =
  push scope_stack (!struct_table, !union_table, !typedef_env, !enum_env)

let scope_leave () =
  let (s, u, t, e) = peek scope_stack in
  struct_table := s;
  union_table := u;
  typedef_env := t;
  enum_env := e;
  pop scope_stack

let const_check e =
  let rec go = function
    | EConst (VInt i) ->
       Some i
    | EArith(op, e1, e2) ->
       opMap2 (arith2fun TInt op) (go e1) (go e2)
    | EUnary(op, e1) ->
       if op = PostInc || op = PostDec then None else
       opMap (unary2fun op) (go e1)
    | _ -> None in
  match go e with
  | Some x -> x
  | None -> failwith "const_check"

let to_unsigned = function
  | (TInt,   true) -> TUInt
  | (TShort, true) -> TUShort
  | (TLong,  true) -> TULong
  | (TChar,  true) -> TUChar
  | (t, _) -> t

let create_type (t1, u1) (t2, u2) =
  let go = function
    | (t1, t2) when not (is_integral t1 && is_integral t2) ->
       failwith "create_type"
    | (TLong, _)  | (_, TLong)  -> TLong
    | (TShort, _) | (_, TShort) -> TShort
    | (TChar, _)  | (_, TChar)  -> TChar
    | _ -> TInt in
  (go (t1, t2), u1 || u2)

%}

%token <int> INT UINT LINT ULINT
%token <float> FLOAT
%token <int list> STR
%token <string> ID
%token <string> TYPEDEF_NAME
%token <string> ENUM_ID
%token TINT TUNSIGNED TSIGNED TFLOAT TCHAR TSHORT TLONG TVOID
%token STRUCT UNION TYPEDEF ENUM
%token STATIC EXTERN
%token IF ELSE WHILE DO FOR
%token RETURN CONTINUE BREAK GOTO
%token SWITCH CASE DEFAULT
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET
%token INC DEC
%token AND OR
%token NOT COND COLON
%token AMP HAT BAR TILDE
%token PLUS MINUS MOD STAR LSHIFT RSHIFT SLASH
%token DOT ARROW
%token EQ NEQ LT LE GT GE
%token SEMICOLON COMMA
%token ASSIGN PLUS_ASSIGN MINUS_ASSIGN
%token STAR_ASSIGN SLASH_ASSIGN MOD_ASSIGN LSHIFT_ASSIGN RSHIFT_ASSIGN
%token AMP_ASSIGN HAT_ASSIGN BAR_ASSIGN
%token SIZEOF
%token DOTS
%token EOF

/* avoid dangling-else problem */
%nonassoc RPAREN
%nonassoc ELSE

%type <Syntax.def list> main
%start main

%%

main:
| external_decl_list EOF
  { List.concat $1 }
| error
  { failwith "parse error" }

external_decl:
| fun_definition
  { [$1] }
| decl
  { List.map (fun x -> DefVar x) $1 }

external_decl_list:
|
  { [] }
| external_decl external_decl_list
  { $1 :: $2 }

fun_definition:
| linkage decl_specs declarator compound_stat
  { DefFun (make_decl $1 $2 ($3, None), get_params $3, $4) }

linkage:
|
  { NoLink }
| EXTERN
  { Extern }
| STATIC
  { Static }

decl_list:
|
  { [] }
| decl decl_list
  { $1 :: $2 }

decl:
| linkage decl_specs init_declarator_list SEMICOLON
  { make_decls $1 $2 $3 }
| TYPEDEF decl_specs declarator SEMICOLON
  { typedef (make_decl NoLink $2 ($3, None)); [] }

decl_specs:
| decl_specs_sub
  { to_unsigned $1 }
| TYPEDEF_NAME
  { List.assoc $1 !typedef_env }

decl_specs_sub:
| type_spec
  { $1 }
| type_spec decl_specs_sub
  { create_type $1 $2 }

type_spec:
| TINT
  { (TInt, false) }
| TSIGNED
  { (TInt, false) }
| TSHORT
  { (TShort, false) }
| TLONG
  { (TLong, false) }
| TUNSIGNED
  { (TInt, true) }
| TCHAR
  { (TChar, false) }
| TFLOAT
  { (TFloat, false) }
| TVOID
  { (TVoid, false) }
| struct_spec
  { ($1, false) }
| enum_spec
  { ($1, false) }

ident_option:
|
  { None }
| ident
  { Some $1 }

struct_spec:
| STRUCT ident_option LBRACE struct_decl_list RBRACE
  { make_structty (List.concat $4) $2 }
| STRUCT ident
  { lookup_structty $2 }
| UNION ident_option LBRACE struct_decl_list RBRACE
  { make_unionty (List.concat $4) $2 }
| UNION ident
  { lookup_unionty $2 }

struct_decl:
| decl
  { $1 }

struct_decl_list:
| struct_decl
  { [$1] }
| struct_decl struct_decl_list
  { $1 :: $2 }

enum_spec:
| ENUM ident_option LBRACE enumerator_list RBRACE
  { make_enumty $4; TInt }
| ENUM ident
  { TInt }

enumerator:
| ident
  { ($1, None) }
| ident ASSIGN const_expr
  { ($1, Some $3) }

enumerator_list:
| enumerator
  { [$1] }
| enumerator COMMA enumerator_list
  { $1 :: $3 }

init_declarator:
| declarator
  { ($1, None) }
| declarator ASSIGN initializer_
  { ($1, Some $3) }

init_declarator_list:
|
  { [] }
| init_declarator
  { [$1] }
| init_declarator COMMA init_declarator_list
  { $1 :: $3 }

declarator:
| direct_declarator
  { $1 }
| STAR declarator
  { DeclPtr $2 }

direct_declarator:
| ident
  { DeclIdent $1 }
| LPAREN id_declarator RPAREN
  { $2 }
| direct_declarator LBRACKET const_expr RBRACKET
  { DeclArray($1, $3) }
| direct_declarator LBRACKET RBRACKET
  { DeclArray($1, 0) }
| direct_declarator LPAREN param_decl_list RPAREN
  { DeclFun ($1, $3)}
| direct_declarator LPAREN RPAREN
  { DeclFun ($1, [])}

id_declarator:
| direct_id_declarator
  { $1 }
| STAR declarator
  { DeclPtr $2 }

direct_id_declarator:
| ID
  { DeclIdent $1 }
| ENUM_ID
  { DeclIdent $1 }
| LPAREN id_declarator RPAREN
  { $2 }
| direct_id_declarator LBRACKET const_expr RBRACKET
  { DeclArray($1, $3) }
| direct_id_declarator LBRACKET RBRACKET
  { DeclArray($1, 0) }
| direct_id_declarator LPAREN param_decl_list RPAREN
  { DeclFun ($1, $3)}
| direct_id_declarator LPAREN RPAREN
  { DeclFun ($1, [])}

param_decl_list:
| param_decl
  { [$1] }
| param_decl COMMA DOTS
  { [$1] }
| param_decl COMMA param_decl_list
  { $1 :: $3 }

param_decl:
| decl_specs declarator
  { make_decl NoLink $1 ($2, None) }
| decl_specs abstract_declarator
  { make_decl NoLink $1 ($2, None) }
| decl_specs
  { make_decl NoLink $1 (DeclIdent "", None) }

initializer_: /* 'initializer' is an OCaml's keyword! */
| assign_expr
  { IScal $1 }
| LBRACE initializer_list RBRACE
  { IVect $2 }

initializer_list:
| initializer_
  { [$1] }
| initializer_ COMMA initializer_list
  { $1 :: $3 }

type_name:
| decl_specs
  { make_type $1 (DeclIdent "") }
| decl_specs abstract_declarator
  { make_type $1 $2 }

abstract_declarator:
| STAR
  { DeclPtr (DeclIdent "") }
| STAR abstract_declarator
  { DeclPtr $2 }
| direct_abstract_declarator
  { $1 }

direct_abstract_declarator:
| LPAREN abstract_declarator RPAREN
  { $2 }
| LBRACKET const_expr RBRACKET
  { DeclArray (DeclIdent "", $2) }
| LPAREN param_decl_list RPAREN
  { DeclFun (DeclIdent "", $2) }
| direct_abstract_declarator LBRACKET RBRACKET
  { DeclArray ($1, 0) }
| direct_abstract_declarator LBRACKET const_expr RBRACKET
  { DeclArray ($1, $3) }
| direct_abstract_declarator LPAREN param_decl_list RPAREN
  { DeclFun ($1, $3) }

stat:
| expr_stat
  { $1 }
| compound_stat
  { $1 }
| selection_stat
  { $1 }
| iteration_stat
  { $1 }
| jump_stat
  { $1 }
| labeled_stat
  { $1 }

stat_list:
|
  { [] }
| stat stat_list
  { $1 :: $2 }

expr_stat:
| SEMICOLON
  { SNil }
| expr SEMICOLON
  { SExpr($1) }

compound_stat:
| lbrace_with_action decl_list stat_list rbrace_with_action
  { SBlock(List.concat $2, $3) }

lbrace_with_action:
| LBRACE
  { scope_enter () }

rbrace_with_action:
| RBRACE
  { scope_leave () }

selection_stat:
| IF LPAREN expr RPAREN stat
  { SIfElse($3, $5, SNil) }
| IF LPAREN expr RPAREN stat ELSE stat
  { SIfElse($3, $5, $7) }
| SWITCH LPAREN expr RPAREN stat
  { SSwitch($3, $5) }

iteration_stat:
| WHILE LPAREN expr RPAREN stat
  { SWhile($3, $5) }
| DO stat WHILE LPAREN expr RPAREN SEMICOLON
  { SDoWhile($2, $5) }
| FOR LPAREN expr_option SEMICOLON expr_option SEMICOLON expr_option RPAREN stat
  { SFor($3, $5, $7, $9) }

jump_stat:
| GOTO ident SEMICOLON
  { SGoto $2 }
| BREAK SEMICOLON
  { SBreak }
| CONTINUE SEMICOLON
  { SContinue }
| RETURN expr_option SEMICOLON
  { SReturn $2 }

labeled_stat:
| ID COLON stat
  { SLabel($1, $3) }
| CASE cond_expr COLON
  { SCase($2) }
| DEFAULT COLON
  { SDefault }

expr:
| assign_expr
  { $1 }
| expr COMMA assign_expr
  { EComma($1, $3) }

expr_option:
|
  { None }
| expr
  { Some $1 }

assign_expr:
| cond_expr
  { $1 }
| unary_expr ASSIGN assign_expr
  { EAssign(None, $1, $3) }
| unary_expr PLUS_ASSIGN assign_expr
  { EAssign(Some Add, $1, $3) }
| unary_expr MINUS_ASSIGN assign_expr
  { EAssign(Some Sub, $1, $3) }
| unary_expr STAR_ASSIGN assign_expr
  { EAssign(Some Mul, $1, $3) }
| unary_expr SLASH_ASSIGN assign_expr
  { EAssign(Some Div, $1, $3) }
| unary_expr MOD_ASSIGN assign_expr
  { EAssign(Some Mod, $1, $3) }
| unary_expr LSHIFT_ASSIGN assign_expr
  { EAssign(Some LShift, $1, $3) }
| unary_expr RSHIFT_ASSIGN assign_expr
  { EAssign(Some RShift, $1, $3) }
| unary_expr AMP_ASSIGN assign_expr
  { EAssign(Some BitAnd, $1, $3) }
| unary_expr HAT_ASSIGN assign_expr
  { EAssign(Some BitXor, $1, $3) }
| unary_expr BAR_ASSIGN assign_expr
  { EAssign(Some BitOr, $1, $3) }

cond_expr:
| logor_expr
  { $1 }
| logor_expr COND expr COLON cond_expr
  { ECond($1, $3, $5) }

const_expr:
| cond_expr
  { const_check $1 }

logor_expr:
| logand_expr
  { $1 }
| logor_expr OR logand_expr
  { ELog (LogOr, $1, $3) }

logand_expr:
| bitor_expr
  { $1 }
| logand_expr AND bitor_expr
  { ELog (LogAnd, $1, $3) }

bitor_expr:
| bitxor_expr
  { $1 }
| bitor_expr BAR bitxor_expr
  { EArith (BitOr, $1, $3) }

bitxor_expr:
| bitand_expr
  { $1 }
| bitxor_expr HAT bitand_expr
  { EArith (BitXor, $1, $3) }

bitand_expr:
| equal_expr
  { $1 }
| bitand_expr AMP equal_expr
  { EArith (BitAnd, $1, $3) }

equal_expr:
| rel_expr
  { $1 }
| equal_expr EQ rel_expr
  { EEq(Eq, $1, $3) }
| equal_expr NEQ rel_expr
  { EEq(Ne, $1, $3) }

rel_expr:
| shift_expr
  { $1 }
| rel_expr LT shift_expr
  { ERel(Lt, $1, $3)}
| rel_expr GT shift_expr
  { ERel(Gt, $1, $3)}
| rel_expr LE shift_expr
  { ERel(Le, $1, $3)}
| rel_expr GE shift_expr
  { ERel(Ge, $1, $3)}

shift_expr:
| additive_expr
  { $1 }
| shift_expr LSHIFT additive_expr
  { EArith(LShift, $1, $3)}
| shift_expr RSHIFT additive_expr
  { EArith(RShift, $1, $3)}

additive_expr:
| multiplicative_expr
  { $1 }
| additive_expr PLUS multiplicative_expr
  { EArith(Add, $1, $3)}
| additive_expr MINUS multiplicative_expr
  { EArith(Sub, $1, $3)}

multiplicative_expr:
| cast_expr
  { $1 }
| multiplicative_expr STAR cast_expr
  { EArith(Mul, $1, $3)}
| multiplicative_expr SLASH cast_expr
  { EArith(Div, $1, $3)}
| multiplicative_expr MOD cast_expr
  { EArith(Mod, $1, $3)}

cast_expr:
| unary_expr
  { $1 }
| LPAREN type_name RPAREN cast_expr
  { ECast($2, $4) }

unary_expr:
| postfix_expr
  { $1 }
| INC unary_expr
  { EAssign(Some Add, $2, EConst(VInt(1))) }
| DEC unary_expr
  { EAssign(Some Sub, $2, EConst(VInt(1))) }
| NOT cast_expr
  { EUnary (LogNot, $2) }
| PLUS cast_expr
  { EUnary (Plus, $2) }
| MINUS cast_expr
  { EUnary (Minus, $2) }
| STAR cast_expr
  { EPtr $2 }
| AMP cast_expr
  { EAddr $2 }
| TILDE cast_expr
  { EUnary (BitNot, $2) }
| SIZEOF LPAREN type_name RPAREN
  { ESizeof $3 }
| SIZEOF unary_expr
  { ESizeofExpr $2 }

postfix_expr:
| primary_expr
  { $1 }
| postfix_expr LBRACKET expr RBRACKET
  { EPtr(EArith(Add, $1, $3)) }
| postfix_expr INC
  { EUnary (PostInc, $1) }
| postfix_expr DEC
  { EUnary (PostDec, $1) }
| postfix_expr LPAREN arg_expr_list RPAREN
  { ECall($1, $3) }
| postfix_expr DOT ident
  { EDot($1, $3) }
| postfix_expr ARROW ident
  { EDot(EPtr $1, $3) }

primary_expr:
| INT
  { EConst (VInt $1) }
| UINT
  { ECast (TUInt, EConst (VInt $1)) }
| LINT
  { ECast (TLong, EConst (VInt $1)) }
| ULINT
  { ECast (TULong, EConst (VInt $1)) }
| FLOAT
  { EConst (VFloat $1) }
| string_literal
  { EConst (VStr $1) }
| ID
  { EVar $1 }
| LPAREN expr RPAREN
  { $2 }
| ENUM_ID
  { EConst (VInt (get_enum $1)) }

string_literal:
| STR
  { $1 }
| STR string_literal
  { (take (List.length $1 - 1) $1) @ $2 }

arg_expr_list:
|
  { [] }
| assign_expr
  { [$1] }
| assign_expr COMMA arg_expr_list
  { $1 :: $3 }

ident:
| ID
  { $1 }
| TYPEDEF_NAME
  { $1 }
| ENUM_ID
  { $1 }
