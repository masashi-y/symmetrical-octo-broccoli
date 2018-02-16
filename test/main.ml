
open Logic

let () =
    if Array.length Sys.argv > 1 then begin
    let str = Sys.argv.(1) in
    print_endline str;
    let term = Logic.parse str in
    print_endline (Term.show term);
    print_endline (Term.show (Term.beta_reduce term));
    print_endline ""
    end


let () =
    let open Term in
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

