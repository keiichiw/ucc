%{
  open Syntax
%}

%token <int> INT
%token <string> ID
%token TINT
%token IF ELSE RETURN WHILE FOR
%token LPAREN RPAREN
%token LBRACE RBRACE
%token PLUS MINUS MOD
%token EQ NEQ LT LE GT GE
%token SEMICOLON COMMA
%token SUBST
%token EOF

%nonassoc below_COMMA
%left COMMA
%right SUBST
%left EQ NEQ
%left LT LE GT GE
%left PLUS MINUS
%right MOD
%start <Syntax.decl list> main

%%

main:
| decl* EOF {$1}


decl:
| decl_var { $1 }
| decl_fun  { $1}

/*
  variables | prototype declaration
 */
decl_var:
// e.g. int x, y;
| t= typeref; vlist= separated_nonempty_list(COMMA, ID); SEMICOLON
  {
    DVars(t,
          List.map
            (fun x -> DeclIdent (Name x))
            vlist,
          ($startpos, $endpos))
  }

// e.g. int f(int x, int y);
| t=typeref; x=ID; LPAREN; tlist= separated_list(COMMA, fun_arg); RPAREN; SEMICOLON
  {
    let tys = List.map (fun x -> TInt) tlist in
    let name = Name x in
    DVars(t,
          [DeclFProto(DeclIdent name, tys)],
          ($startpos, $endpos))
  }



decl_fun: //e.g. int f(int x) {statement}
| t=typeref; name=ID; LPAREN; a=separated_list(COMMA, fun_arg); RPAREN; b=block
  {DFun(t, Name name, a, b, ($startpos, $endpos))}


fun_arg:
| ty=typeref; name=ID { (ty, (Name name)) }

typeref:
| TINT { TInt }

block:
| LBRACE; l=lvar*; s=stmt*; RBRACE { Block (List.concat l, s) }

lvar: // local variables
| t= typeref; vlist= separated_nonempty_list(COMMA, ID); SEMICOLON
  {List.map (fun x -> SVar (t, Name x)) vlist}

stmt: // statement
| SEMICOLON {SNil}
| expr SEMICOLON {SExpr($1)}
| WHILE LPAREN expr RPAREN block {SWhile($3, $5)}
| FOR LPAREN e1= expr?; SEMICOLON e2= expr?; SEMICOLON e3=expr?; RPAREN; b=block
  {SFor(e1, e2, e3, b)}
| IF LPAREN expr RPAREN block { SIfElse($3, $5, (Block ([],[]))) }
| IF LPAREN expr RPAREN block ELSE block {SIfElse($3, $5, $7)}
| RETURN expr SEMICOLON {SReturn $2}

expr:
| simple_expr %prec below_COMMA
    { $1 }
| simple_expr COMMA expr
    { EComma($1, $3) }

simple_expr:
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
| expr GT expr
    { ELt($3, $1)}
| expr LE expr
    { ELe($1, $3)}
| expr GE expr
    { ELe($3, $1)}
| ID SUBST expr
    { ESubst(Name $1, $3) }
| ID LPAREN args RPAREN
    { EApp(Name $1, $3) }
| ID
    { EVar (Name $1)}
| value
    { EConst($1)}

value:
| INT {VInt($1)}

args:
| simple_expr
    {[$1]}
| simple_expr COMMA args
    {$1::$3}
