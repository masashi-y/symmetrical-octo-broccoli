%{
open Logic
open Logic.Term
%}

%token TRUE FALSE
%token NEG
%token CONJ DISJ IMPL EQUAL
%token FORALL EXISTS LAMBDA
%token PERIOD COMMA
%token LPAREN RPAREN
%token EOF
%token <string> VAR
%start <Logic.Term.t> main

%%

main : e=expr EOF          { e }
expr : e=expr6             { e }

lambda :
| LAMBDA xs=nonempty_list(VAR) PERIOD e=expr6 { List.fold_right (fun v b -> Lambda (v, b)) xs e }

expr6 :
| e=lambda                     { e }
| FORALL x=VAR PERIOD e=expr6  { Forall (x, e) }
| EXISTS x=VAR PERIOD e=expr6  { Exists (x, e) }
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
| f=func xs=args            { List.fold_left (fun f x -> App (f, x)) f xs }
| TRUE                     { True }
| FALSE                    { False }
| x=VAR                    { Var x }
| NEG e=expr0              { Not e }
| LPAREN e=expr RPAREN     { e }

func :
| LPAREN e=lambda RPAREN { e }
| f=VAR     { Var f }

args :
| LPAREN xs=separated_list(COMMA, expr) RPAREN     { xs }

%%
