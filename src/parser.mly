%{
open Logic
open Logic.AST

let mkApp f xs = List.fold_left (fun f x -> App (f, x)) f xs
%}

%token TRUE FALSE
%token NEG
%token CONJ DISJ IMPL EQUAL
%token FORALL EXISTS LAMBDA
%token PERIOD COMMA
%token LPAREN RPAREN
%token EOF
%token <string> VAR
%start <Logic.AST.t> main

%%

main : e=expr EOF          { e }
expr : e=expr6             { e }

lambda :
| LAMBDA xs=nonempty_list(VAR) PERIOD e=expr6
    { List.fold_right (fun v b -> Lambda (v, b)) xs e }

expr6 :
| e=lambda { e }
| e=expr5  { e }

expr5 :
| n=option(NEG) FORALL x=VAR PERIOD e=expr5
    { match n with Some _ -> Not (Forall (x, e)) | None -> Forall (x, e) }
| n=option(NEG) EXISTS x=VAR PERIOD e=expr5
    { match n with Some _ -> Not (Exists (x, e)) | None -> Exists (x, e) }
| e=expr4                     { e }
                           
expr4 :
| e1=expr4 IMPL e2=expr3   { Imp (e1, e2) }
| e=expr3                  { e }

expr3 :
| e1=expr3 DISJ e2=expr2   { Or (e1, e2) }
| e=expr2                  { e }

expr2 :
| e1=expr2 CONJ e2=expr1   { And (e1, e2) }
| e=expr1                  { e }

expr1 :
| e1=expr1 EQUAL e2=expr0  { Equal (e1, e2) }
| e=expr0                  { e }

expr0 :
| TRUE                     { True }
| FALSE                    { False }
| NEG e=expr0              { Not e }
| f=term xs=option(args)
    { match xs with Some xs -> mkApp f xs | None -> f }
| LPAREN f=lambda RPAREN xs=option(args)
    { match xs with Some xs -> mkApp f xs | None -> f }
| LPAREN e=expr5 RPAREN    { e }

term :
| f=VAR     { Term f }

args :
| e=term { [e] }
| LPAREN xs=separated_nonempty_list(COMMA, expr) RPAREN     { xs }

%%
