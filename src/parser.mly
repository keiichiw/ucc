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

let struct_table : (string * int) list ref = ref []
let union_table  : (string * int) list ref = ref []

let get_ty = function
  | Decl (_, TArray (ty, sz), _, _) ->
     TPtr ty
  | Decl (_, ty, _, _) ->
     ty

let make_decl ln ty (decl, exp) =
  let name = ref (Name "") in
  let ty =
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
    go id decl in
  Decl (ln, ty, !name, exp)

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
  (match make_decl NoLink ty (decl, None) with
   | Decl (_, ty, Name "", None) ->
      ty
   | _ ->
      raise (ParserError "make_type"))

let lookup_structty name =
  try
    List.assoc name !struct_table
  with
  | Not_found ->
     let sid = List.length !struct_env in
     struct_table := (name, sid) :: !struct_table;
     struct_env := [] :: !struct_env; (* push a dummy *)
     sid

let lookup_unionty name =
  try
    List.assoc name !union_table
  with
  | Not_found ->
     let uid = List.length !union_env in
     union_table := (name, uid) :: !union_table;
     union_env := [] :: !union_env; (* push a dummy *)
     uid

let insert_struct_decl sid decl =
  let rec go = function
    | [], _ -> failwith "insert_struct_decl"
    | _ :: xs, i when i = sid -> decl :: xs
    | x :: xs, i -> x :: go (xs, (i-1)) in
  struct_env := go (!struct_env, List.length !struct_env - 1)

let insert_union_decl uid decl =
  let rec go = function
    | [], _ -> failwith "insert_union_decl"
    | _ :: xs, i when i = uid -> decl :: xs
    | x :: xs, i -> x :: go (xs, (i-1)) in
  union_env := go (!union_env, List.length !union_env - 1)

let make_structty name_opt decl =
  let go (Decl (_, ty, Name n, _)) = (n, ty) in
  let decl = List.map go decl in
  match name_opt with
  | Some name ->
     let sid = lookup_structty name in
     insert_struct_decl sid decl;
     TStruct sid
  | None ->
     let sid = List.length !struct_env in
     struct_env := decl :: !struct_env;
     TStruct sid

let make_unionty name_opt decl =
  let go (Decl (_, ty, Name n, _)) = (n, ty) in
  let decl = List.map go decl in
  match name_opt with
  | Some name ->
     let uid = lookup_unionty name in
     insert_union_decl uid decl;
     TUnion uid
  | None ->
     let uid = List.length !union_env in
     union_env := decl :: !union_env;
     TUnion uid

let make_enumty enums =
  let go num = function
    | (enum, Some cnst) ->
       enum_def enum cnst;
       cnst+1
    | (enum, None) ->
       enum_def enum num;
       num+1 in
  let _ = List.fold_left go 0 enums in ()

let rec fold_expr = function
  | EConst (VInt i) ->
     Some i
  | EArith(op, e1, e2) ->
     opMap2 (arith2fun op) (fold_expr e1) (fold_expr e2)
  | ERel (op, e1, e2) ->
     opMap2 (rel2fun op) (fold_expr e1) (fold_expr e2)
  | EEq (op, e1, e2) ->
     opMap2 (eq2fun op) (fold_expr e1) (fold_expr e2)
  | EUnary(op, e1) ->
     opMap (unary2fun op) (fold_expr e1)
  | ECond (e1, e2, e3) ->
     if fold_expr e1 != Some 0 then fold_expr e2 else fold_expr e3
  | ELog (LogAnd, e1, e2) ->
     if fold_expr e1 != Some 0 then fold_expr e2 else Some 0
  | ELog (LogOr, e1, e2) ->
     if fold_expr e1 != Some 0 then fold_expr e1 else fold_expr e2
  | _ -> None

let rec const_fold e =
  match fold_expr e with
  | Some x -> x
  | None -> failwith "const_fold"

let rec init_fold e =
  match fold_expr e with
  | Some x -> EConst (VInt x)
  | None -> e

let epilogue () =
  struct_env := List.rev !struct_env;
  union_env := List.rev !union_env

let to_unsigned = function
  | _ -> TUInt

let create_type = function
  | (t, TUInt)
  | (TUInt, t) ->
     to_unsigned t
  | (_, TLong)
  | (TLong, _) ->
     TLong
  | (_, TShort)
  | (TShort, _) ->
     TShort
  | (_, TChar)
  | (TChar, _) ->
     TChar
  | _ ->
     TInt
%}

%token <int> INT
%token <int> UINT
%token <float> FLOAT
%token <int list> STR
%token <string> ID
%token <string> TYPEDEF_NAME
%token <string> ENUM_ID
%token TINT TUNSIGNED TFLOAT TCHAR TSHORT TLONG TVOID
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
  { epilogue (); List.concat $1 }
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

decl:
| decl_real SEMICOLON
  { $1 }

decl_list:
|
  { [] }
| decl decl_list
  { $1 :: $2 }

decl_real:
| linkage decl_specs init_declarator_list
  { List.map (make_decl $1 $2) $3 }
| TYPEDEF type_spec declarator
  { typedef (make_decl NoLink $2 ($3, None)); [] }

decl_specs:
| type_spec
  { $1 }
| type_spec decl_specs
  { create_type ($1, $2) }

type_spec:
| TINT
  { TInt }
| TSHORT
  { TShort }
| TLONG
  { TLong }
| TUNSIGNED
  { TUInt }
| TCHAR
  { TChar }
| TFLOAT
  { TFloat}
| TVOID
  { TVoid }
| TYPEDEF_NAME
    { List.assoc $1 !typedef_env }
| struct_spec
  { $1 }
| enum_spec
  { $1 }

ident_option:
|
  { None }
| ident
  { Some $1 }

struct_spec:
| STRUCT ident_option LBRACE struct_decl_list RBRACE
  { make_structty $2 (List.concat $4) }
| STRUCT ident
  { TStruct (lookup_structty $2) }
| UNION ident_option LBRACE struct_decl_list RBRACE
  { make_unionty $2 (List.concat $4) }
| UNION ident
  { TUnion (lookup_unionty $2) }

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
| ID
  { DeclIdent(Name $1) }
| LPAREN declarator RPAREN
  { $2 }
| direct_declarator LBRACKET const_expr RBRACKET
  { DeclArray($1, $3) }
| direct_declarator LBRACKET RBRACKET
  { DeclArray($1, 0) }
| direct_declarator LPAREN param_decl_list RPAREN
  { DeclFun ($1, $3)}
| direct_declarator LPAREN RPAREN
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
  { make_decl NoLink $1 (DeclIdent (Name ""), None) }

initializer_: /* 'initializer' is an OCaml's keyword! */
| assign_expr
  { IScal (init_fold $1) }
| LBRACE initializer_list RBRACE
  { IVect $2 }

initializer_list:
| initializer_
  { [$1] }
| initializer_ COMMA initializer_list
  { $1 :: $3 }

type_name:
| decl_specs
  { make_type $1 (DeclIdent (Name "")) }
| decl_specs abstract_declarator
  { make_type $1 $2 }

abstract_declarator:
| STAR
  { DeclPtr (DeclIdent (Name "")) }
| STAR abstract_declarator
  { DeclPtr $2 }
| direct_abstract_declarator
  { $1 }

direct_abstract_declarator:
| LPAREN abstract_declarator RPAREN
  { $2 }
| LBRACKET const_expr RBRACKET
  { DeclArray (DeclIdent (Name ""), $2) }
| LPAREN param_decl_list RPAREN
  { DeclFun (DeclIdent (Name ""), $2) }
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
| LBRACE decl_list stat_list RBRACE
  { SBlock(List.concat $2, $3) }

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
| CASE const_expr COLON
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
  { const_fold $1 }

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
  { EDot($1, Name $3) }
| postfix_expr ARROW ident
  { EDot(EPtr $1, Name $3) }

primary_expr:
| INT
  { EConst (VInt $1) }
| FLOAT
  { EConst (VFloat $1) }
| UINT
  { ECast (TUInt, EConst (VInt $1)) }
| string_literal
  { EConst (VStr $1) }
| ID
  { EVar (Name $1)}
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
