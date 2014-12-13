%{
  open Syntax
  open Parser_helper
  exception ParserError of string
  exception Unreachable of string
  let struct_num = ref 0;;
  type declarator =
    | DeclPtr of declarator
    | DeclIdent  of name * (expr option)
    | DeclArray  of declarator * size
    | DeclFun of declarator * (dvar list)
  let rec make_dvar ty = function
    | DeclPtr d ->
       make_dvar (TPtr ty) d
    | DeclIdent (name, exp) ->
       DVar(ty, name, exp)
    | DeclArray (d, sz) ->
       (match make_dvar ty d with
        | DVar(typ, name, None) ->
           DVar (TArray(typ, sz), name, None)
        | _ -> raise (ParserError "make_dvar: array"))
    | DeclFun (d, dvs) ->
       (match make_dvar ty d with
        | DVar(typ, name, None) ->
           DVar (TFun(typ, dvs), name, None)
        | _ -> raise (ParserError "make_dvar: fun"))
%}

%token <int> INT
%token <string> ID
%token <string> TYPEDEF_NAME
%token TINT
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
%token EOF

(* avoid dangling-else problem *)
%nonassoc RPAREN
%nonassoc ELSE

%start <Syntax.def list> main

%%

main:
| external_decl* EOF { List.concat $1 }

external_decl:
| function_definition  { [$1] }
| decl { List.map (fun x -> DefVar x) $1 }

function_definition:
| typ=decl_specs d=declarator b=compound_stat?
  {
    match b with
    | Some(SBlock (ds, ss)) ->
       DefFun (make_dvar typ d, Block(ds, ss))
    | None ->
       raise (ParserError "fun_definition: proto")
    | _ -> raise (Unreachable "fun_definition")
  }

decl: // local variables
| typ=decl_specs; dlist=separated_list(COMMA, init_declarator); SEMICOLON
  { List.map (make_dvar typ) dlist }
| TYPEDEF ty=type_spec d=declarator SEMICOLON
  {
    let DVar(typ, Name name, _) = make_dvar ty d in
    typedef_env := (name, typ) :: !typedef_env;
    []
  }

decl_specs:
| type_spec
  { $1 }

type_spec:
| TINT
  { TInt }
| TYPEDEF_NAME
  { List.assoc $1 !typedef_env }
| struct_spec
  { $1 }

struct_spec:
| STRUCT name=ID LBRACE l=separated_nonempty_list(SEMICOLON, struct_decl) RBRACE
  {
    let snum = !struct_num in
    struct_num := !struct_num + 1;
    struct_table := (name, snum)::!struct_table;
    struct_env := (snum, List.concat l)::!struct_env;
    TStruct(snum)
  }
| STRUCT LBRACE l=separated_nonempty_list(SEMICOLON, struct_decl) RBRACE
  {
    let snum = !struct_num in
    struct_num := !struct_num + 1;
    struct_env := (snum, List.concat l)::!struct_env;
    TStruct(snum)
  }
| STRUCT id=ID
  { TStruct (List.assoc id !struct_table)}

struct_decl:
| decl+
  { List.concat $1 }

declarator:
| direct_declarator
  { $1 }
| STAR declarator
  { DeclPtr $2 }

direct_declarator:
| ID
  { DeclIdent(Name $1, None) }
| LPAREN declarator RPAREN
  { $2 }
| d=direct_declarator LBRACKET i=INT RBRACKET
  { DeclArray(d, i) }
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
  { make_dvar $1 $2 }
init_declarator:
| declarator
  { $1 }
| declarator ASSIGN assign_expr
  {
    match $1 with
    | DeclIdent(name, _) ->
       DeclIdent(name, Some $3)
    | _ ->
       raise (ParserError "array initializer is unsupported")
  }

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
| CASE cond_expr COLON
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
  { EAssign($1, EAdd($1, $3)) }
| unary_expr MINUS_ASSIGN assign_expr
  { EAssign($1, ESub($1, $3)) }
| unary_expr STAR_ASSIGN assign_expr
  { EAssign($1, EApp(EVar(Name "__mul"), [$1;$3])) }
| unary_expr SLASH_ASSIGN assign_expr
  { EAssign($1, EApp(EVar(Name "__div"), [$1;$3])) }
| unary_expr MOD_ASSIGN assign_expr
  { EAssign($1, EApp(EVar(Name "__mod"), [$1;$3])) }
| unary_expr LSHIFT_ASSIGN assign_expr
  { EAssign($1, EShift($1, $3)) }
| unary_expr RSHIFT_ASSIGN assign_expr
  { EAssign($1, EShift($1, ESub(EConst(VInt 0), $3))) }
| unary_expr AMP_ASSIGN assign_expr
  { EAssign($1, EApp(EVar(Name "__and"), [$1;$3])) }
| unary_expr HAT_ASSIGN assign_expr
  { EAssign($1, EApp(EVar(Name "__xor"), [$1;$3])) }
| unary_expr BAR_ASSIGN assign_expr
  { EAssign($1, EApp(EVar(Name "__or"), [$1;$3])) }

cond_expr:
| logor_expr
  { $1 }
| logor_expr COND expr COLON cond_expr
  { ECond($1, $3, $5) }

logor_expr:
| logand_expr
  { $1 }
| logor_expr OR logand_expr
  { EOr ($1, $3) }

logand_expr:
| bitor_expr
  { $1 }
| logand_expr AND bitor_expr
  { EAnd ($1, $3) }

bitor_expr:
| bitxor_expr
  { $1 }
| bitor_expr BAR bitxor_expr
  { EApp(EVar(Name "__or" ), [$1;$3]) }

bitxor_expr:
| bitand_expr
  { $1 }
| bitxor_expr HAT bitand_expr
  { EApp(EVar(Name "__xor"), [$1;$3]) }

bitand_expr:
| equal_expr
  { $1 }
| bitand_expr AMP equal_expr
  { EApp(EVar(Name "__and"), [$1;$3]) }

equal_expr:
| rel_expr
  { $1 }
| equal_expr EQ rel_expr
  { EEq($1, $3) }
| equal_expr NEQ rel_expr
  { ENeq($1, $3)}

rel_expr:
| shift_expr
  { $1 }
| rel_expr LT shift_expr
  { ELe(EConst(VInt 1), ESub($3, $1)) }
| rel_expr GT shift_expr
  { ELe(EConst(VInt 1), ESub($1, $3)) }
| rel_expr LE shift_expr
  { ELe($1, $3)}
| rel_expr GE shift_expr
  { ELe($3, $1)}

shift_expr:
| additive_expr
  { $1 }
| shift_expr LSHIFT additive_expr
  { EShift($1, $3)}
| shift_expr RSHIFT additive_expr
  { EShift($1, ESub(EConst(VInt 0), $3))}

additive_expr:
| multiplicative_expr
  { $1 }
| additive_expr PLUS multiplicative_expr
  { EAdd($1, $3)}
| additive_expr MINUS multiplicative_expr
  { ESub($1, $3)}

multiplicative_expr:
| cast_expr
  { $1 }
| multiplicative_expr STAR cast_expr
  { EApp(EVar(Name "__mul"), [$1;$3]) }
| multiplicative_expr SLASH cast_expr
  { EApp(EVar(Name "__div"), [$1;$3]) }
| multiplicative_expr MOD cast_expr
  { EApp(EVar(Name "__mod"), [$1;$3]) }

cast_expr:
| unary_expr
  { $1 }

unary_expr:
| postfix_expr
  { $1 }
| INC unary_expr
  { EAssign($2, EAdd($2, EConst(VInt(1)))) }
| DEC unary_expr
  { EAssign($2, ESub($2, EConst(VInt(1)))) }
| NOT unary_expr
  { EEq(EConst(VInt 0), $2) }
| PLUS unary_expr
  { $2 }
| MINUS unary_expr
  { ESub(EConst(VInt 0), $2) }
| STAR unary_expr
  { EPtr $2 }
| AMP unary_expr
  { EAddr $2 }
| TILDE unary_expr
  { EApp(EVar(Name "__not"), [$2]) }

postfix_expr:
| primary_expr
  { $1 }
| v=postfix_expr LBRACKET e=expr RBRACKET
  { EArray(v, e) }
| postfix_expr INC
  (* i++ -> (++i,i-1) *)
  { EComma(EAssign($1, EAdd($1, EConst(VInt(1)))), ESub($1, EConst(VInt(1)))) }
| postfix_expr DEC
  (* i-- -> (--i,i+1) *)
  { EComma(EAssign($1, ESub($1, EConst(VInt(1)))), EAdd($1, EConst(VInt(1)))) }
| postfix_expr LPAREN arg_expr_list RPAREN
  { EApp($1, $3) }
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
