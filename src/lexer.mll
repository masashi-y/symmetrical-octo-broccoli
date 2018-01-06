{
  open Parser

  exception Error of string
}

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
| [' ' '\t' '\n'] (* also ignore newlines, not only whitespace and tabs *)
    { token lexbuf }
| "True" | "TrueP"
    { TRUE }
| "False"
    { FALSE }
| '-' | '~'
    { NEG } 
| '&'
    { CONJ }
| '\\'
    { LAMBDA }
| '|'
    { DISJ }
| "->"
    { IMPL }
| '='
    { EQUAL }
| "forall"
    { FORALL }
| "exists"
    { EXISTS }
| '.'
    { PERIOD }
| ','
    { COMMA }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| id { VAR (Lexing.lexeme lexbuf) }
| eof
    { EOF }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
