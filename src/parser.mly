%{
  open Syntax
  open Ctype
  open Parser_helper
  exception ParserError of string
  exception Unreachable of string
  let struct_num = ref 0;;
  type declarator =
    | DeclPtr of declarator
    | DeclIdent  of name
    | DeclArray  of declarator * size
    | DeclFun of declarator * (decl list)
  let get_ty = function
    | Decl (ty, _, _) -> ty
  let rec make_decl ty (decl, exp) =
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
      go (fun x -> x) decl in
    Decl (ty, !name, exp)
  let get_params = function
    | DeclFun (_, dvs) -> dvs
    | _ -> raise (ParserError "get_params")
  let make_type ty decl =
    (match make_decl ty (decl, None) with
     | Decl (ty, Name "", None) ->
        ty
     | _ ->
        raise (ParserError "make_type"))
  let make_structty name_opt decl =
    let snum = !struct_num in
    struct_num := !struct_num + 1;
    (match name_opt with
     | Some name ->
        struct_table := (name, snum)::!struct_table
     | None -> ());
    struct_env := (snum, decl)::!struct_env;
    TStruct(snum)
  let rec fold_expr = function
    | EConst (VInt i) -> i
    | EArith(Add ,e1, e2) -> (fold_expr e1) + (fold_expr e2)
    | EArith(Sub, e1, e2) -> (fold_expr e1) - (fold_expr e2)
    | ERel (Lt, e1, e2) -> if (fold_expr e1) < (fold_expr e2) then 1 else 0
    | EEq (Eq, e1, e2) -> if (fold_expr e1) = (fold_expr e2) then 1 else 0
    | EEq (Ne, e1, e2) -> if (fold_expr e1) != (fold_expr e2) then 1 else 0
    | ECond (e1, e2, e3) -> if fold_expr e1 != 0 then fold_expr e2 else fold_expr e3
    | ELog (And, e1, e2) -> if fold_expr e1 != 0 then fold_expr e2 else 0
    | ELog (Or, e1, e2) -> if fold_expr e1 != 0 then fold_expr e1 else fold_expr e2
    | _ -> raise (ParserError "fold_expr")
%}

%token <int> INT
%token <string> ID
%token <string> TYPEDEF_NAME
%token TINT TUNSIGNED
%token STRUCT TYPEDEF
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
%token EOF

(* avoid dangling-else problem *)
%nonassoc RPAREN
%nonassoc ELSE

%start <Syntax.def list> main

%%

main:
| external_decl* EOF
  { List.concat $1 }

external_decl:
| function_definition
  { [$1] }
| decl
  { List.map (fun x -> DefVar x) $1 }

function_definition:
| typ=decl_specs d=declarator b=compound_stat
  { DefFun (make_decl typ (d,None), get_params d, b) }

real_decl:
| typ=decl_specs; dlist=separated_list(COMMA, init_declarator)
  { List.map (make_decl typ) dlist }
| TYPEDEF ty=type_spec d=declarator
  { typedef (make_decl ty (d, None)); [] }

decl:
| real_decl SEMICOLON
  { $1 }

decl_specs:
| type_spec
  { $1 }

type_spec:
| TINT
  { TInt }
| TUNSIGNED
  { TUnsigned }
| TYPEDEF_NAME
    { List.assoc $1 !typedef_env }
| struct_spec
  { $1 }

struct_spec:
| STRUCT ID? LBRACE struct_decl+ RBRACE
  { make_structty $2 (List.concat $4) }
| STRUCT ID
  { TStruct (List.assoc $2 !struct_table)}

struct_decl:
| decl
  { $1 }

init_declarator:
| declarator
  { ($1, None) }
| declarator ASSIGN initializer_
  { ($1, Some $3) }

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
| direct_declarator LPAREN param_decl_list RPAREN
  { DeclFun ($1, $3)}
| direct_declarator LPAREN RPAREN
  { DeclFun ($1, [])}

param_decl_list:
| param_decl
  { [$1] }
| param_decl COMMA param_decl_list
  { $1::$3 }

param_decl:
| decl_specs declarator
  { make_decl $1 ($2, None) }

initializer_: /* 'initializer' is an OCaml's keyword! */
| assign_expr
  { IScal $1 }
| LBRACE l=separated_list(COMMA, initializer_) RBRACE
  { IList l }

type_name:
| type_spec
  { make_type $1 (DeclIdent (Name "")) }
| type_spec abstract_declarator
  { make_type $1 $2 }

abstract_declarator:
| STAR
  { DeclPtr (DeclIdent (Name "")) }
| STAR direct_abstract_declarator
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

expr_stat:
| SEMICOLON
  { SNil }
| expr SEMICOLON
  { SExpr($1) }

compound_stat:
| LBRACE decl* stat* RBRACE
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
| FOR LPAREN e1=expr?; SEMICOLON e2=expr?; SEMICOLON e3=expr?; RPAREN stat
  { SFor(e1, e2, e3, $9) }

jump_stat:
| GOTO ID SEMICOLON
  { SGoto $2 }
| BREAK SEMICOLON
  { SBreak }
| CONTINUE SEMICOLON
  { SContinue }
| RETURN expr SEMICOLON
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

assign_expr:
| cond_expr
  { $1 }
| unary_expr ASSIGN assign_expr
  { EAssign($1, $3) }
| unary_expr PLUS_ASSIGN assign_expr
  { EAssign($1, EArith(Add, $1, $3)) }
| unary_expr MINUS_ASSIGN assign_expr
  { EAssign($1, EArith(Sub, $1, $3)) }
| unary_expr STAR_ASSIGN assign_expr
  { EAssign($1, EArith(Mul, $1, $3)) }
| unary_expr SLASH_ASSIGN assign_expr
  { EAssign($1, EArith(Div, $1, $3)) }
| unary_expr MOD_ASSIGN assign_expr
  { EAssign($1, EArith(Mod, $1, $3)) }
| unary_expr LSHIFT_ASSIGN assign_expr
  { EAssign($1, EArith(LShift, $1, $3)) }
| unary_expr RSHIFT_ASSIGN assign_expr
  { EAssign($1, EArith(RShift, $1, $3)) }
| unary_expr AMP_ASSIGN assign_expr
  { EAssign($1, EArith(BitAnd, $1, $3)) }
| unary_expr HAT_ASSIGN assign_expr
  { EAssign($1, EArith(BitXor, $1, $3)) }
| unary_expr BAR_ASSIGN assign_expr
  { EAssign($1, EArith(BitOr, $1, $3)) }

cond_expr:
| logor_expr
  { $1 }
| logor_expr COND expr COLON cond_expr
  { ECond($1, $3, $5) }

const_expr:
| cond_expr
  { fold_expr $1 }

logor_expr:
| logand_expr
  { $1 }
| logor_expr OR logand_expr
  { ELog (Or, $1, $3) }

logand_expr:
| bitor_expr
  { $1 }
| logand_expr AND bitor_expr
  { ELog (And, $1, $3) }

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
  { EAssign($2, EArith(Add, $2, EConst(VInt(1)))) }
| DEC unary_expr
  { EAssign($2, EArith(Sub, $2, EConst(VInt(1)))) }
| NOT unary_expr
  { EUnary (LogNot, $2) }
| PLUS unary_expr
  { EUnary (Plus, $2) }
| MINUS unary_expr
  { EUnary (Minus, $2) }
| STAR unary_expr
  { EPtr $2 }
| AMP unary_expr
  { EAddr $2 }
| TILDE unary_expr
  { EUnary (BitNot, $2) }
| SIZEOF LPAREN type_name RPAREN
  { ESizeof $3 }

postfix_expr:
| primary_expr
  { $1 }
| postfix_expr LBRACKET expr RBRACKET
  { EArray($1, $3) }
| postfix_expr INC
  { EUnary (PostInc, $1) }
| postfix_expr DEC
  { EUnary (PostDec, $1) }
| postfix_expr LPAREN arg_expr_list RPAREN
  { ECall($1, $3) }
| postfix_expr DOT ID
  { EDot($1, Name $3) }
| postfix_expr ARROW ID
  { EDot(EPtr $1, Name $3) }

primary_expr:
| constant_expr
  { $1 }
| ID
  { EVar (Name $1)}
| LPAREN expr RPAREN
  { $2 }

arg_expr_list:
| args=separated_list(COMMA, assign_expr)
  { args }

constant_expr:
| INT
  { EConst(VInt $1) }
