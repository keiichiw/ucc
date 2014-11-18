%{
  open Syntax
%}

%token <int> INT
%token <string> ID
%token TINT
%token IF ELSE RETURN WHILE FOR
%token LPAREN RPAREN
%token LBRACE RBRACE
%token PLUS MINUS MOD EQ NEQ LT
%token SEMICOLON COMMA
%token SUBST
%token EOF

%left EQ NEQ
%left LT
%left PLUS MINUS
%right MOD
%start <Syntax.decl list> main

%%

main:
| decl* EOF {$1}


decl:
| decl_fun  { $1}
| decl_var { $1 }


decl_var:
| t= typeref; vlist= separated_nonempty_list(COMMA, ID); SEMICOLON
  { DVars(t,
          List.map
            (fun x -> DeclIdent (Name x))
            vlist,
          ($startpos, $endpos))
  }
| t=typeref; x=ID; LPAREN; tlist= separated_nonempty_list(COMMA, typeref); RPAREN; SEMICOLON
  { let tys = List.map (fun x -> TInt) tlist in
    let name = Name x in
    DVars(t,
          [DeclFProto(DeclIdent name, tys)],
          ($startpos, $endpos))
  }

decl_fun:
| t=typeref; name=ID; LPAREN; p=params; RPAREN; b=block
  {DFun(t, Name name, p, b, ($startpos, $endpos))}

params:
| typeref ID {[($1, (Syntax.Name $2))]}

typeref:
| TINT { TInt }

block:
| LBRACE s=stmt*; RBRACE {s}

stmt:
| SEMICOLON {SNil}
| expr SEMICOLON {SExpr($1)}
| t= typeref; vlist= separated_nonempty_list(COMMA, ID); SEMICOLON
  {SVars(t, List.map (fun x -> Name x) vlist, ($startpos, $endpos))}
| WHILE LPAREN expr RPAREN block {SWhile($3, $5)}
| FOR LPAREN el1= separated_list(COMMA, expr); SEMICOLON e2= expr?; SEMICOLON
  el3=separated_list(COMMA, expr); RPAREN; b=block
  {SFor(el1, e2, el3, b)}
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
| expr NEQ expr
    { ENeq($1, $3)}
| expr LT expr
    { ELt($1, $3)}
| expr SUBST expr
    { ESubst($1, $3)}
| ID LPAREN args RPAREN
    { EApp(Name $1, $3) }
| ID
    { EVar (Name $1)}
| value
    { EConst($1)}
value:
| INT {VInt($1)}

args:
| expr
    {[$1]}
| expr COMMA args
    {$1::$3}
