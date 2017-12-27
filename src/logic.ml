
open Utils

module Term = struct

    type var = string

    type t = True
           | False
           | Var of var
           | Const of string
           | Not of t
           | Equal of t * t
           | And of t * t
           | Or of t * t
           | Imp of t * t
           | App of t * t
           | Lambda of var * t
           | Exists of var * t
           | Forall of var * t

    let rec show = function
        | True           -> "True"
        | False          -> "False"
        | Var v          -> v
        | Const s        -> s
        | Not t          -> !%"~(%s)" (show t)
        | Equal (t1, t2) -> !%"%s = %s" (show t1) (show t2)
        | And (t1, t2)   -> !%"(%s /\\ %s)" (show t1) (show t2)
        | Or (t1, t2)    -> !%"(%s \\/ %s)" (show t1) (show t2)
        | Imp (t1, t2)   -> !%"%s -> %s" (show t1) (show t2)
        | App (Var f, t1) ->
                !%"%s(%s)" f (show t1)
        | App (App (Var f, t1), t2) ->
                !%"%s(%s, %s)" f (show t1) (show t2)
        | App (App (App (Var f, t1), t2), t3) ->
                !%"%s(%s, %s, %s)" f (show t1) (show t2) (show t3)
        | App (App (App (App (Var f, t1), t2), t3), t4) ->
                !%"%s(%s, %s, %s, %s)" f (show t1) (show t2) (show t3) (show t4)
        | App (t1, t2)  -> !%"%s (%s)" (show t1) (show t2)
        | Lambda (v, t) -> !%"\\%s. (%s)" (v) (show t)
        | Exists (v, t) -> !%"exists %s. (%s)" (v) (show t)
        | Forall (v, t) -> !%"forall %s. (%s)" (v) (show t)
end

let () =
    let open Term in
    let test = Not (Exists ("e", And ((App (Const "eat", Var "e")), Equal (App (Const "Subj", Var "e"), Const "taro")))) in
    let test1 = (App (Const "eat", Var "e")) in
    let test = Imp (test, test1) in
    print_endline (show test)
