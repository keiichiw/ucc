%{
  open Syntax
  exception ParserError of string
  let rec getNameFromDecl = function
    | DeclIdent (n, _) -> n
    | DeclArray (x, _) -> getNameFromDecl x
  let rec nestPtr t = function
      | 0 -> t
      | i -> TPtr (nestPtr t (i-1))
%}

%token <int> INT
%token <string> ID
%token TINT
%token IF ELSE WHILE DO FOR
%token RETURN CONTINUE BREAK
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET
%token INC DEC
%token NOT COND COLON
%token AMP HAT BAR TILDE
%token PLUS MINUS MOD STAR LSHIFT RSHIFT SLASH
%token EQ NEQ LT LE GT GE
%token SEMICOLON COMMA
%token SUBST PLUSSUBST MINUSSUBST
%token STARSUBST SLASHSUBST MODSUBST LSHIFTSUBST RSHIFTSUBST
%token AMPSUBST HATSUBST BARSUBST
%token EOF


%start <Syntax.def list> main

%%

main:
| top_decl* EOF { $1 }


top_decl:
| fun_definition  { $1 }

// int f (int x, int y) {...}
fun_definition:
| ty=decl_specifier; f=declarator; LPAREN; dlist=separated_list(COMMA, arg_declaration);  RPAREN; b=block
  {
    let (starNum, decl) = f in
    let nm = getNameFromDecl decl in
    let typ = nestPtr ty starNum in
    DefFun (typ, nm, dlist, b,  ($startpos, $endpos))
  }

decl_specifier:
| TINT
  { TInt }


declarator:
| direct_declarator
  { (0, $1) }
| STAR d=declarator
  {
    let (x, y) = d in
    (x+1, y)
  }

direct_declarator:
| ID
  { DeclIdent(Name $1, None) }
| d=direct_declarator LBRACKET i=INT RBRACKET
  { DeclArray(d, i) }

init_declarator:
| declarator
  { $1 }
| declarator SUBST assign_expr
  {
    let (num, decl) = $1 in
    match decl with
    | DeclIdent(name, _) ->
       (num, DeclIdent(name, Some $3))
    | DeclArray(d, i) ->
       raise (ParserError "array initializer is unsupported")
  }

block:
| LBRACE; l=declaration_stmt*; s=stmt*; RBRACE
  { Block (List.concat l, s) }

declaration_stmt:
| declaration SEMICOLON
  { $1 }


arg_declaration:
| ty=decl_specifier; d=declarator
  {
    let rec f (starNum,decl) =
      let typ = nestPtr ty starNum in
      let rec sizeOf = function
        | DeclIdent(_) -> 1
        | DeclArray(d,i) ->
           (sizeOf d) * i in
      match decl with
      | DeclIdent(name, exp) ->
         DVar(typ, name, exp)
      | DeclArray(d, i) ->
         DArray(typ, getNameFromDecl d, sizeOf decl) in
    f d
  }

declaration: // local variables
| ty=decl_specifier; dlist=separated_list(COMMA, init_declarator)
  {
    let rec f (starNum,decl) =
      let typ = nestPtr ty starNum in
      let rec sizeOf = function
        | DeclIdent(_) -> 1
        | DeclArray(d,i) -> (sizeOf d) * i in
      match decl with
      | DeclIdent(name, exp) ->
         DVar(typ, name, exp)
      | DeclArray(d, i) ->
         DArray(typ, getNameFromDecl d, sizeOf decl) in
    List.map f dlist
  }

stmt: // statement
| SEMICOLON
  { SNil }
| expr SEMICOLON
  { SExpr($1) }
| WHILE LPAREN expr RPAREN block
  { SWhile($3, $5) }
| DO b=block WHILE LPAREN e=expr RPAREN SEMICOLON
  { SDoWhile( b, e) }
| FOR LPAREN e1= expr?; SEMICOLON e2= expr?; SEMICOLON e3=expr?; RPAREN; b=block
  { SFor(e1, e2, e3, b) }
| IF LPAREN expr RPAREN block
  { SIfElse($3, $5, (Block ([],[]))) }
| IF LPAREN expr RPAREN block ELSE block
  { SIfElse($3, $5, $7) }
| CONTINUE SEMICOLON
  { SContinue }
| BREAK SEMICOLON
  { SBreak }
| RETURN expr SEMICOLON
  { SReturn $2 }

expr:
| assign_expr
  { $1 }
| expr COMMA assign_expr
  { EComma($1, $3) }

assign_expr:
| cond_expr
  { $1 }
| unary_expr SUBST assign_expr
  { ESubst($1, $3) }
| unary_expr PLUSSUBST assign_expr
  { ESubst($1, EAdd($1, $3)) }
| unary_expr MINUSSUBST assign_expr
  { ESubst($1, ESub($1, $3)) }
| unary_expr STARSUBST assign_expr
  { ESubst($1, EApp(Name "__mul", [$1;$3])) }
| unary_expr SLASHSUBST assign_expr
  { ESubst($1, EApp(Name "__div", [$1;$3])) }
| unary_expr MODSUBST assign_expr
  { ESubst($1, EApp(Name "__mod", [$1;$3])) }
| unary_expr LSHIFTSUBST assign_expr
  { ESubst($1, EShift($1, $3)) }
| unary_expr RSHIFTSUBST assign_expr
  { ESubst($1, EShift($1, ESub(EConst(VInt 0), $3))) }
| unary_expr AMPSUBST assign_expr
  { ESubst($1, EApp(Name "__and", [$1;$3])) }
| unary_expr HATSUBST assign_expr
  { ESubst($1, EApp(Name "__xor", [$1;$3])) }
| unary_expr BARSUBST assign_expr
  { ESubst($1, EApp(Name "__or", [$1;$3])) }

cond_expr:
| logor_expr
  { $1 }
| logor_expr COND expr COLON cond_expr
  { ECond($1, $3, $5) }

logor_expr:
| logand_expr
  { $1 }

logand_expr:
| bitor_expr
  { $1 }

bitor_expr:
| bitxor_expr
  { $1 }
| bitor_expr BAR bitxor_expr
  { EApp(Name "__or", [$1;$3]) }

bitxor_expr:
| bitand_expr
  { $1 }
| bitxor_expr HAT bitand_expr
  { EApp(Name "__xor", [$1;$3]) }

bitand_expr:
| equal_expr
  { $1 }
| bitand_expr AMP equal_expr
  { EApp(Name "__and", [$1;$3]) }

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
  { EApp(Name "__mul", [$1;$3]) }
| multiplicative_expr SLASH cast_expr
  { EApp(Name "__div", [$1;$3]) }
| multiplicative_expr MOD cast_expr
  { EApp(Name "__mod", [$1;$3]) }

cast_expr:
| unary_expr
  { $1 }

unary_expr:
| NOT unary_expr
  { EEq(EConst(VInt 0), $2) }
| PLUS unary_expr
  { $2 }
| MINUS unary_expr
  { ESub(EConst(VInt 0), $2) }
| INC unary_expr
  { ESubst($2, EAdd($2, EConst(VInt(1)))) }
| DEC unary_expr
  { ESubst($2, ESub($2, EConst(VInt(1)))) }
| STAR unary_expr
  { EPtr $2 }
| AMP unary_expr
  { EAddr $2 }
| TILDE unary_expr
  { EApp(Name "__not", [$2]) }
| postfix_expr
  { $1 }

postfix_expr:
| primary
  { $1 }
| p=postfix_expr LPAREN args=separated_list(COMMA, assign_expr);RPAREN
  {
    match p with
    | EVar name -> EApp(name, args)
    | _ -> raise (ParserError "postfix: function application")
  }
| p=postfix_expr INC
  {
    (* i++ -> (++i,i-1) *)
    EComma( ESubst(p, EAdd(p, EConst(VInt(1)))),
            ESub(p, EConst(VInt(1))))
  }
| p=postfix_expr DEC
  {
    (* i-- -> (--i,i+1) *)
    EComma( ESubst(p, ESub(p, EConst(VInt(1)))),
            EAdd(p, EConst(VInt(1))))
  }
| p=postfix_expr LBRACKET e=expr;RBRACKET
  { EPtr(EAdd(p, e)) }
primary:
| INT
  { EConst(VInt $1) }
| ID
  { EVar (Name $1)}
| LPAREN expr RPAREN
  { $2 }
