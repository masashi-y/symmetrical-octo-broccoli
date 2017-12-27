
open Logic

let main () =
  (* let input = open_in filename in *)
  (* let filebuf = Lexing.from_input input in *)
  let str = Sys.argv.(1) in
  let filebuf = Lexing.from_string str in
  print_endline str;
  try
    print_endline (Term.show (Parser.main Lexer.token filebuf))
  with
  | Lexer.Error msg ->
      Printf.eprintf "%s%!" msg
  | Parser.Error ->
      Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start filebuf)

let _ = main ()
