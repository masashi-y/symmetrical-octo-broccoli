
open Logic

let main () =
  (* let input = open_in filename in *)
  (* let filebuf = Lexing.from_input input in *)
  let str = Sys.argv.(1) in
  let filebuf = Lexing.from_string str in
  print_endline str;
  try
      let ast = Parser.main Lexer.token filebuf in
    print_endline (Term.show (Term.from_AST ast));
    print_endline (Term.show (Term.beta_reduce (Term.from_AST ast)));
    print_endline "";
  print_endline (AST.show ast)
  with
  | Lexer.Error msg ->
      Printf.eprintf "%s%!" msg
  | Parser.Error ->
      Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start filebuf)

let _ = main ()
(*
let () =
    let open AST in
    let filebuf = Lexing.from_string "~(exists e. eat(e) & (exists x. x = taro & Subj(e) = x))" in
    let test = (Parser.main Lexer.token filebuf) in
    let test1 = (App (Term "eat", Term "e")) in
    let test = Imp (test, test1) in
    print_endline (show test);
    print_endline (Term.show (Term.from_AST test));
    *)
