{
  open Parser

  exception Error of string
}

let digit = ['0'-'9']
let space = ' ' | '\t'
let alpha = ['a'-'z' 'A'-'Z' '_' ]
let ident = alpha (alpha | digit)*

rule token = parse
| space
    { token lexbuf }
| ['\r' '\n']
    { Lexing.new_line lexbuf; token lexbuf }
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
| "for"
    { FOR }
| "return"
    { RETURN }
| '+'
    { PLUS }
| '-'
    { MINUS }
| '*'
    { STAR }
| '%'
    { MOD }
| '&'
    { AMP }
| "=="
    { EQ}
| "!="
    { NEQ}
| "="
    { SUBST }
| "+="
    { PLUSSUBST }
| "-="
    { MINUSSUBST }
| "<"
    { LT }
| ">"
    { GT }
| "<="
    { LE }
| ">="
    { GE }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '{'
    { LBRACE }
| '}'
    { RBRACE }
| "//"
    { commentbis lexbuf }
| "/*"
    { comment lexbuf }
| digit+ as i
    { INT (int_of_string i) }
| ident  as n
    { ID n }
| eof
    { EOF }
| _
    { raise (Error
               (Printf.sprintf "At offset %d: unexpected character.\n"
                               (Lexing.lexeme_start lexbuf)))
    }

and comment = parse
| "*/"
    { token lexbuf }
| _
    { comment lexbuf }
| eof
    { raise (Error "Error: unclosed comment\n") }

and commentbis = parse
| '\n'
    { Lexing.new_line lexbuf; token lexbuf }
| _
    { commentbis lexbuf }
| eof
    { EOF }
