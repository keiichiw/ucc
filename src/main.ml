
(* the name of the file which contains the expressions *)

let out_file fname =
  let fnlen = String.length fname in
  let suffix = if 2 < fnlen then String.sub fname (fnlen - 2) 2 else "" in
  if suffix = ".c" || suffix = ".i" then
    Printf.sprintf "%s.s" (String.sub fname 0 (fnlen - 2))
  else
    Printf.sprintf "%s.s" fname

let show_pos fname filebuf =
  let pos = filebuf.Lexing.lex_start_p in
  Printf.eprintf "File \"%s\", line %d, character %d:\n"
    fname
    pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let show_error fmt =
  let go str =
    Printf.eprintf "error: %s\n" str;
    exit 1 in
  Printf.ksprintf go fmt

let () =
  let argc = Array.length Sys.argv in
  if argc != 2 then Format.printf "Usage: ./cc [filename]\n" else
  let fname = Sys.argv.(1) in
  let inchan = open_in fname in
  let filebuf = Lexing.from_channel inchan in
  try
    let parse = Parser.main Lexer.token filebuf in
    let typing = Typing.main parse in
    let outchan = open_out (out_file fname) in
    Emitter.main outchan typing;
  with
  | Lexer.LexerError msg ->
    show_pos fname filebuf;
    show_error "lex: %s" msg
  | Failure "parse error" ->
    show_pos fname filebuf;
    show_error "parser: syntax error near '%s'" (Lexing.lexeme filebuf)
