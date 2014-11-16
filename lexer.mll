{
  open Parser

  exception Error of string
}

let digit = ['0'-'9']
let space = ' ' | '\t' | '\r' | '\n'
let alpha = ['a'-'z' 'A'-'Z' '_' ]
let ident = alpha (alpha | digit)*

rule token = parse
| [' ' '\t' '\n']
    { token lexbuf }
| ';'
    { SEMICOLON }
| ','
    { COMMA}
| "int"
    { TINT }
| "if"
    { IF }
| "else"
    { ELSE }
| "while"
    { WHILE }
| "return"
    { RETURN }
| '+'
    { PLUS }
| '-'
    { MINUS }
| '%'
    { MOD }
| "=="
    { EQ}
| "="
    { SUBST }
| "<"
    { LT }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '{'
    { LBRACE }
| '}'
    { RBRACE }
| digit+ as i
    { INT (int_of_string i) }
| ident  as n
    { ID n }
| eof
    { EOF }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
