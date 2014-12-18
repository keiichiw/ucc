{
  open Parser
  open Parser_helper
  exception Error of string

  let cast_char_to_int s =
    let table = [
      ('a', 7); ('b', 8); ('t', 9); ('n', 10);
      ('v', 11); ('f', 12); ('r', 13); ('"', 34);
      ('\'', 39); ('?', 63); ('\\', 92);
    ] in
    if s.[0] <> '\\' then
      Char.code(s.[0])
    else try
      List.assoc s.[1] table
    with Not_found ->
      let len = String.length s in
      if s.[1] = 'x' then
        int_of_string ("0x" ^ String.sub s 2 (len - 2))
      else
        int_of_string ("0o" ^ String.sub s 1 (len - 1))
}

let digit = ['0'-'9']
let dec = ['1'-'9'] digit*
let oct = '0' ['0'-'7']*
let hex = '0' ['x' 'X'] ['0'-'9' 'a'-'f' 'A'-'F']+
let bin = '0' ['b' 'B'] ['0' '1']+
let space = ' ' | '\t'
let alpha = ['a'-'z' 'A'-'Z' '_' ]
let ident = alpha (alpha | digit)*
let escapes = ['a' 'b' 't' 'n' 'v' 'f' 'r' '"' '\'' '?' '\\']
let char = [^'\\'] | '\\' (escapes | ['0'-'7']+ | 'x' ['0'-'9' 'a'-'f' 'A'-'F']+)

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
| "unsigned"
    { TUNSIGNED }
| "char"
    { TCHAR }
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
    { EQ }
| "!="
    { NEQ }
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
| "sizeof"
    { SIZEOF }
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
| '\'' (char as c) '\''
    { INT (cast_char_to_int c) }
| '\"'
    { STR (string_elements lexbuf) }
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

and string_elements = parse
| '\"'
    { [0] }
| char as c
    { (cast_char_to_int c)::(string_elements lexbuf) }

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
