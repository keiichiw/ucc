%{
  open Syntax
  open Parser_helper
  exception ParserError of string
  exception Unreachable of string
  let struct_num = ref 0;;
  type declarator =
    | DeclPtr of declarator
    | DeclIdent  of name
    | DeclArray  of declarator * size
    | DeclFun of declarator * (dvar list)
  let rec make_dvar ty (decl, exp) =
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
           go (fun x -> TFun (k x, dvs)) d in
      go (fun x -> x) decl in
    DVar (ty, !name, exp)
  let make_structty name_opt decl =
    let snum = !struct_num in
    struct_num := !struct_num + 1;
    (match name_opt with
     | Some name ->
        struct_table := (name, snum)::!struct_table
     | None -> ());
    struct_env := (snum, decl)::!struct_env;
    TStruct(snum)
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
| external_decl* EOF
  { List.concat $1 }

external_decl:
| function_definition
  { [$1] }
| decl
  { List.map (fun x -> DefVar x) $1 }

function_definition:
| typ=decl_specs d=declarator b=compound_stat
  { DefFun (make_dvar typ (d,None), b) }

decl:
| typ=decl_specs; dlist=separated_list(COMMA, init_declarator); SEMICOLON
  { List.map (make_dvar typ) dlist }
| TYPEDEF ty=type_spec d=declarator SEMICOLON
  { typedef (make_dvar ty (d, None)); [] }

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
| declarator ASSIGN assign_expr
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
| direct_declarator LBRACKET INT RBRACKET
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
  { make_dvar $1 ($2, None) }

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
