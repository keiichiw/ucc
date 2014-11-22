open Print

(* the name of the file which contains the expressions *)

let out_file fname =
  let fnlen = String.length fname in
  if 2<fnlen && (String.sub fname (fnlen-2) 2) = ".c" then
    Format.sprintf "%s.s" (String.sub fname 0 (fnlen-2))
  else
    "out.s"

let main () =
  let argc = Array.length Sys.argv in
  if argc != 2 then
    Format.printf "Usage: ./ucc [filename]\n"
  else
    let fname = Sys.argv.(1) in
    let inchan  = open_in fname in
    let outchan = open_out (out_file fname) in
    let filebuf = Lexing.from_channel inchan in
    try
      let parse = Parser.main Lexer.token filebuf in
      Emit.main outchan parse;
    with
    | Lexer.Error msg ->
       Printf.eprintf "%s%!" msg
    | Parser.Error ->
       Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start filebuf)
      ;
      close_in inchan;
      close_out outchan

let _ = main ()
