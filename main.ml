open Batteries
open Format
open Print

(* the name of the file which contains the expressions *)
let filename = Sys.argv.(1)

let print_int_list = List.print Int.print stdout

let main () =
  let input = open_in filename in
  let filebuf = Lexing.from_input input in
  try
    print_program std_formatter (Parser.main Lexer.token filebuf)
  with
  | Lexer.Error msg ->
      Printf.eprintf "%s%!" msg
  | Parser.Error ->
      Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start filebuf)
  ;
  IO.close_in input

let _ = main ()
