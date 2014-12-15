{
  open Parser
  open Parser_helper
  exception Error of string
}

let digit = ['0'-'9']
let dec = ['1'-'9'] digit*
let oct = '0' ['0'-'7']*
let hex = '0' ['x' 'X'] ['0'-'9' 'a'-'f' 'A'-'F']+
let bin = '0' ['b' 'B'] ['0' '1']+
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
| "struct"
    { STRUCT }
| "typedef"
    { TYPEDEF }
| "if"
    { IF }
| "else"
    { ELSE }
| "while"
    { WHILE }
| "do"
    { DO }
| "for"
    { FOR }
| "return"
    { RETURN }
| "continue"
    { CONTINUE }
| "break"
    { BREAK }
| "goto"
    { GOTO }
| "switch"
    { SWITCH }
| "case"
    { CASE }
| "default"
    { DEFAULT }
| '+'
    { PLUS }
| "++"
    { INC }
| '-'
    { MINUS }
| '!'
    { NOT }
| '?'
    { COND }
| ':'
    { COLON }
| "--"
    { DEC }
| '*'
    { STAR }
| '/'
    { SLASH }
| '%'
    { MOD }
| "<<"
    { LSHIFT }
| ">>"
    { RSHIFT }
| '.'
    { DOT }
| "->"
    { ARROW }
| '&'
    { AMP }
| '^'
    { HAT }
| '|'
    { BAR }
| "&&"
    { AND }
| "||"
    { OR }
| '~'
    { TILDE }
| "=="
    { EQ}
| "!="
    { NEQ}
| "="
    { ASSIGN }
| "+="
    { PLUS_ASSIGN }
| "-="
    { MINUS_ASSIGN }
| "*="
    { STAR_ASSIGN }
| "/="
    { SLASH_ASSIGN }
| "%="
    { MOD_ASSIGN }
| "<<="
    { LSHIFT_ASSIGN }
| ">>="
    { RSHIFT_ASSIGN }
| "&="
    { AMP_ASSIGN }
| "^="
    { HAT_ASSIGN }
| "|="
    { BAR_ASSIGN }
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
| '['
    { LBRACKET }
| ']'
    { RBRACKET }
| "//"
    { commentbis lexbuf }
| "#"
    { commentbis lexbuf }
| "/*"
    { comment lexbuf }
| bin as i
    { INT (int_of_string i) }
| dec as i
    { INT (int_of_string i) }
| oct as i
    { INT (int_of_string ("0o"^i)) }
| hex as i
    { INT (int_of_string i) }
| ident  as n
    {
      if is_typedef_name n then
        TYPEDEF_NAME n
      else
        ID n
    }
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
