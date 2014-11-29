%{
  open Syntax
  exception ParserError of string
  let rec getNameFromDecl = function
    | DeclIdent n -> n
    | DeclArray (x, _) -> getNameFromDecl x
  let rec nestPtr t = function
      | 0 -> t
      | i -> TPtr (nestPtr t (i-1))
%}

%token <int> INT
%token <string> ID
%token TINT
%token IF ELSE RETURN WHILE FOR
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET
%token INC DEC
%token AMP HAT BAR TILDE
%token PLUS MINUS MOD STAR LSHIFT RSHIFT SLASH
%token EQ NEQ LT LE GT GE
%token SEMICOLON COMMA
%token SUBST PLUSSUBST MINUSSUBST
%token EOF


%nonassoc below_COMMA
%left COMMA
%right SUBST PLUSSUBST MINUSSUBST
%left BAR
%left HAT
%left AMP
%left EQ NEQ
%left LT LE GT GE
%left LSHIFT RSHIFT
%left PLUS MINUS
%right MOD STAR SLASH
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
  { DeclIdent(Name $1) }
| d=direct_declarator LBRACKET i=INT RBRACKET
  { DeclArray(d, i) }



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
      | DeclIdent(name) ->
         DVar(typ, name)
      | DeclArray(d, i) ->
         DArray(typ, getNameFromDecl d, sizeOf d) in
    f d
  }

declaration: // local variables
| ty=decl_specifier; dlist=separated_list(COMMA, declarator)
  {
    let rec f (starNum,decl) =
      let typ = nestPtr ty starNum in
      let rec sizeOf = function
        | DeclIdent(_) -> 1
        | DeclArray(d,i) ->
           (sizeOf d) * i in
      match decl with
      | DeclIdent(name) ->
         DVar(typ, name)
      | DeclArray(d, i) ->
         DArray(typ, getNameFromDecl d, sizeOf d) in
    List.map f dlist
  }

stmt: // statement
| SEMICOLON
  { SNil }
| expr SEMICOLON
  { SExpr($1) }
| WHILE LPAREN expr RPAREN block
  { SWhile($3, $5) }
| FOR LPAREN e1= expr?; SEMICOLON e2= expr?; SEMICOLON e3=expr?; RPAREN; b=block
  { SFor(e1, e2, e3, b) }
| IF LPAREN expr RPAREN block
  { SIfElse($3, $5, (Block ([],[]))) }
| IF LPAREN expr RPAREN block ELSE block
  { SIfElse($3, $5, $7) }
| RETURN expr SEMICOLON
  { SReturn $2 }

expr:
| simple_expr %prec below_COMMA
  { $1 }
| simple_expr COMMA expr
  { EComma($1, $3) }

simple_expr:
| expr PLUS expr
  { EAdd($1, $3)}
| expr LSHIFT expr
  { EShift($1, $3)}
| expr RSHIFT expr
  { EShift($1, ESub(EConst(VInt 0), $3))}
| expr MINUS expr
  { ESub($1, $3)}
| expr STAR expr
  { EApp(Name "__mul", [$1;$3]) }
| expr SLASH expr
  { EApp(Name "__div", [$1;$3]) }
| expr MOD expr
  { EApp(Name "__mod", [$1;$3]) }
| expr AMP expr
  { EApp(Name "__and", [$1;$3]) }
| expr BAR expr
  { EApp(Name "__or", [$1;$3]) }
| expr HAT expr
  { EApp(Name "__xor", [$1;$3]) }
| expr EQ expr
  { EEq($1, $3)}
| expr NEQ expr
  { ENeq($1, $3)}
| expr LT expr
  { ELe(EConst(VInt 1), ESub($3, $1)) }
| expr GT expr
  { ELe(EConst(VInt 1), ESub($1, $3)) }
| expr LE expr
  { ELe($1, $3)}
| expr GE expr
  { ELe($3, $1)}
| unary SUBST expr
  { ESubst($1, $3) }
| unary PLUSSUBST expr
  { ESubst($1, EAdd($1, $3)) }
| unary MINUSSUBST expr
  { ESubst($1, ESub($1, $3)) }
| unary
  { $1 }


unary:
| PLUS unary
  { $2 }
| MINUS unary
  { ESub(EConst(VInt 0), $2) }
| INC unary
  { ESubst($2, EAdd($2, EConst(VInt(1)))) }
| DEC unary
  { ESubst($2, ESub($2, EConst(VInt(1)))) }
| STAR unary
  { EPtr $2 }
| AMP unary
  { EAddr $2 }
| TILDE unary
  { EApp(Name "__not", [$2]) }
| postfix_expr
  { $1 }

postfix_expr:
| primary
  { $1 }
| p=postfix_expr LPAREN args=separated_list(COMMA, simple_expr);RPAREN
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
