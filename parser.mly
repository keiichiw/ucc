%{
  open Syntax
%}

%token <int> INT
%token <string> ID
%token TINT
%token IF ELSE RETURN WHILE
%token LPAREN RPAREN
%token LBRACE RBRACE
%token PLUS MINUS MOD EQ LT
%token SEMICOLON
%token SUBST
%token EOF

%left EQ
%left LT
%left PLUS MINUS
%right MOD
%start <Syntax.defin list> main

%%

main:
| top_defs EOF {$1}

top_defs:
| defun  { [$1]}
| defvar { [$1]}
| defun  top_defs  { $1::$2}
| defvar top_defs  { $1::$2}

defvar:
| typeref ID SUBST expr SEMICOLON {DVar($1, Name $2, $4)}

defun:
| typeref ID LPAREN params RPAREN block {DFun($1, Name $2, $4, $6)}

params:
| typeref ID {[($1, (Syntax.Name $2))]}

typeref:
| TINT { TInt }

block:
| LBRACE stmts RBRACE {$2}

stmts:
| stmt {[$1]}
| stmt stmts {$1::$2}

stmt:
| SEMICOLON {SNil}
| expr SEMICOLON {SExpr($1)}
| WHILE LPAREN expr RPAREN block {SWhile($3, $5)}
| IF LPAREN expr RPAREN block {SIf($3, $5)}
| IF LPAREN expr RPAREN block ELSE block {SIfElse($3, $5, $7)}
| RETURN expr SEMICOLON {SReturn $2}

expr:
| LPAREN expr RPAREN
    { $2 }
| expr PLUS expr
    { EAdd($1, $3)}
| expr MINUS expr
    { ESub($1, $3)}
| expr MOD expr
    { EMod($1, $3)}
| expr EQ expr
    { EEq($1, $3)}
| expr LT expr
    { ELt($1, $3)}
| ID
    { EVar (Name $1)}
| value
    { EConst($1)}
value:
| INT {VInt($1)}
