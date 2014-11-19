open Print

(* the name of the file which contains the expressions *)
let filename = Sys.argv.(1)

let main () =
  let inchan  = open_in filename in
  let outchan = open_out ("out.s") in
  let filebuf = Lexing.from_channel inchan in
  try
    let parse = Parser.main Lexer.token filebuf in
    (*print_program stdout parse;*)
    Emit.main stdout parse;
  with
  | Lexer.Error msg ->
      Printf.eprintf "%s%!" msg
  | Parser.Error ->
      Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start filebuf)
  ;
  close_in inchan;
  close_out outchan

let _ = main ()
