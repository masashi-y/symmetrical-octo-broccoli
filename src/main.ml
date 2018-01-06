
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

let () =
    let open Term in
    let parse s = let filebuf = Lexing.from_string s in
        let ast = Parser.main Lexer.token filebuf in
        from_AST ast
    in
    let taro = mkConst "_taro"
    and hit = mkConst "_hit"
    and hanako = mkConst "_hanako"
    and _NP = parse "\\E F1 F2. exists x. (E(x) & F1(x) & F2(x))" (* "\\E x. E(x)" *)
    and _SNPNP = parse "\\E Q1 Q2 K. Q2(\\x.TrueP, \\x.Q1(\\y.TrueP, \\y.exists e.(E(e) & (Subj(e) = x) & (Acc(e) = y) & K(e))))" in
    let taro = mkApp _NP [taro] in
    let hanako = mkApp _NP [hanako] in
    let hit = mkApp _SNPNP [hit] in
    let final = parse "\\S D. S(\\e.TrueP)" in
    let final' = parse "\\L R. L(R,\\e.TrueP)" in
    let term = mkApp (mkApp final [(mkApp (mkApp hit [taro]) [hanako])]) [final'] in
    print_endline (show (normalize @@ beta_reduce term))

