%{
  open Syntax
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
%token PLUS MINUS MOD STAR AMP
%token EQ NEQ LT LE GT GE
%token SEMICOLON COMMA
%token SUBST PLUSSUBST MINUSSUBST
%token EOF


%nonassoc below_COMMA
%left COMMA
%right SUBST PLUSSUBST MINUSSUBST
%left EQ NEQ
%left LT LE GT GE
%left PLUS MINUS
%right MOD
%start <Syntax.decl list> main

%%

main:
| decl* EOF { $1 }


decl:
| decl_vars { $1 }
| decl_fun  { $1 }



/*
  prototype declaration
  TODO: global variables
 */
decl_vars:
// e.g. int f(int x, int y);
| t=prim_type; x=ID; LPAREN; tlist= separated_list(COMMA, fun_arg); RPAREN; SEMICOLON
  {
    let tys = List.map (fun x -> TInt) tlist in
    let name = Name x in
    DVar(t,
         DeclFProto(DeclIdent name, tys),
         ($startpos, $endpos))
  }

///////////

//e.g. int f(int x) {statement}
decl_fun:
| t=prim_type; name=ID; LPAREN; a=separated_list(COMMA, fun_arg); RPAREN; b=block
  {DFun(t, Name name, a, b, ($startpos, $endpos))}


fun_arg:
| ty=prim_type; s=STAR*; name=ID
  {
    let typ = nestPtr ty (List.length s) in
    (typ, (Name name))
  }

prim_type:
| TINT
  { TInt }

block:
| LBRACE; l=lvar*; s=stmt*; RBRACE { Block (List.concat l, s) }

lvar: // local variables
| ty= prim_type; vlist= separated_nonempty_list(COMMA, lvar_def); SEMICOLON
  {
    List.map
      (fun (starnum, name) -> SVar (nestPtr ty starnum, name))
      vlist
  }

lvar_def: // e.g. **a -> (2, Name "a")
| s=STAR* n=ID
  { ( List.length s, Name n)}

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
| unary SUBST expr
  { ESubst($1, $3) }
| unary PLUSSUBST expr
  { ESubst($1, EAdd($1, $3)) }
| unary MINUSSUBST expr
  { ESubst($1, ESub($1, $3)) }
| ID LPAREN args RPAREN
  { EApp(Name $1, $3) }
| unary
  { $1 }

args:
| simple_expr
  {[$1]}
| simple_expr COMMA args
  {$1::$3}

unary:
| PLUS unary
  { $2 }
| MINUS unary
  { ESub(EConst(VInt 0), $2) }
| STAR unary
  { EPtr $2 }
| AMP unary
  { EAddr $2 }
| primary
  { $1 }
primary:
| INT
  { EConst(VInt $1) }
| ID
  { EVar (Name $1)}
| LPAREN expr RPAREN
  { $2 }
