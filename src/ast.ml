
open Utils

type t = True
       | False
       | Term of string
       | Not of t
       | Equal of t * t
       | And of t * t
       | Or of t * t
       | Imp of t * t
       | App of t * t
       | Lambda of string * t
       | Exists of string * t
       | Forall of string * t

let rec show = function
    | True           -> "True"
    | False          -> "False"
    | Term s         -> !%"Term (%s)" s
    | Not t          -> !%"Not (%s)" (show t)
    | Equal (t1, t2) -> !%"Equal (%s, %s)" (show t1) (show t2)
    | And (t1, t2)   -> !%"And (%s, %s)" (show t1) (show t2)
    | Or (t1, t2)    -> !%"Or (%s, %s)" (show t1) (show t2)
    | Imp (t1, t2)   -> !%"Imp (%s, %s)" (show t1) (show t2)
    | App (t1, t2)   -> !%"App (%s, %s)" (show t1) (show t2)
    | Lambda (v, t)  -> !%"Lambda (%s, %s)" (v) (show t)
    | Exists (v, t)  -> !%"Exists (%s, %s)" (v) (show t)
    | Forall (v, t)  -> !%"Forall (%s, %s)" (v) (show t)

