{
open Parser
open Parser_helper
open String

exception LexerError of string

let cast_char_to_int s =
  let table = [
    ('a', 7); ('b', 8); ('t', 9); ('n', 10);
    ('v', 11); ('f', 12); ('r', 13); ('"', 34);
    ('\'', 39); ('?', 63); ('\\', 92);
  ] in
  if s.[0] <> '\\' then
    Char.code s.[0]
  else try
    List.assoc s.[1] table
  with Not_found ->
    let len = String.length s in
    if s.[1] = 'x' then
      int_of_string ("0x" ^ String.sub s 2 (len - 2))
    else
      int_of_string ("0o" ^ String.sub s 1 (len - 1))

let stoi s =
  if String.length s >= 2 && s.[0] = '0' && s.[1] < '8' then
    int_of_string ("0o" ^ s)
  else
    int_of_string s
}

let digit = ['0'-'9']
let dec = ['1'-'9'] digit*
let hex = '0' ['x' 'X'] ['0'-'9' 'a'-'f' 'A'-'F']+
let oct = '0' ['0'-'7']*
let integer = dec | hex | oct
let f = ('f'|'F')
let exp = ['E' 'e'] ['+' '-']? digit+
let float1 = digit+ exp f?
let float2 = digit* '.' digit+ exp? f?
let float3 = digit+ '.' digit* exp? f?
let fnum = float1 | float2 | float3
let space = [' ' '\t' '\r']
let alpha = ['a'-'z' 'A'-'Z' '_' ]
let ident = alpha (alpha | digit)*
let escapes = ['a' 'b' 't' 'n' 'v' 'f' 'r' '"' '\'' '?' '\\']
let char = [^'\\'] | '\\' (escapes | ['0'-'7']+ | 'x' ['0'-'9' 'a'-'f' 'A'-'F']+)

rule token = parse
| space
  { token lexbuf }
| '\n'
  { Lexing.new_line lexbuf; token lexbuf }
| ';'
  { SEMICOLON }
| ','
  { COMMA }
| "int"
  { TINT }
| "long"
  { TLONG }
| "short"
  { TSHORT }
| "signed"
  { TSIGNED }
| "unsigned"
  { TUNSIGNED }
| "char"
  { TCHAR }
| "float"
  { TFLOAT }
| "void"
  { TVOID }
| "struct"
  { STRUCT }
| "union"
  { UNION }
| "enum"
  { ENUM }
| "typedef"
  { TYPEDEF }
| "static"
  { STATIC }
| "extern"
  { EXTERN }
| "const"
  { token lexbuf }
| "volatile"
  { token lexbuf }
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
| "..."
  { DOTS }
| "//"
  { commentbis lexbuf }
| "#"
  { commentbis lexbuf }
| "/*"
  { comment lexbuf }
| integer as i
  { INT (stoi i) }
| (integer as i) ['u' 'U']
  { UINT (stoi i) }
| (integer as i) ['l' 'L']
  { LINT (stoi i) }
| (integer as i) ("ul" | "UL")
  { ULINT (stoi i) }
| '\'' (char as c) '\''
  { INT (cast_char_to_int c) }
| fnum as f
  {
    let len = String.length f in
    let lst = f.[len-1] in
    if lst = 'f' || lst = 'F' then
      FLOAT (float_of_string (String.sub f 0 (len-1)))
    else
      FLOAT (float_of_string f)
  }

| '\"'
  { STR (string_elements lexbuf) }
| ident  as n
  {
    if is_typedef_name n then
      TYPEDEF_NAME n
    else if is_enum_id n then
      ENUM_ID n
    else
      ID n
  }
| eof
  { EOF }
| _
  { raise (LexerError ("illegal token '%s'" ^ Lexing.lexeme lexbuf)) }

and string_elements = parse
| '\"'
  { [0] }
| char as c
  { (cast_char_to_int c)::(string_elements lexbuf) }

and comment = parse
| "*/"
  { token lexbuf }
| '\n'
  { Lexing.new_line lexbuf; comment lexbuf }
| eof
  { raise (LexerError "unterminated comment") }
| _
  { comment lexbuf }

and commentbis = parse
| '\n'
  { Lexing.new_line lexbuf; token lexbuf }
| eof
  { EOF }
| _
  { commentbis lexbuf }
