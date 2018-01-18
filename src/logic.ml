
open Utils

module List = struct
    include List

    let index v =
        let rec f v n = function
            | [] -> raise Not_found
            | v' :: vs when v = v' -> n
            | v' :: vs -> f v (n+1) vs
        in f v 0
end

module StateM : sig
    type ('a, 'b) t
    val bind : ('a, 'c) t -> ('a -> ('b, 'c) t) -> ('b, 'c) t
    val (>>=) : ('a, 'c) t -> ('a -> ('b, 'c) t) -> ('b, 'c) t

    (* these two functions does not pass state transitions *)
    val (&&&) : ('a -> ('b, 'c) t) -> ('e -> ('d, 'c) t) -> (('a * 'e) -> (('b * 'd), 'c) t)
    val sequence' : ('a, 'b) t list -> ('a list, 'b) t

    val return : 'a -> ('a, 'b) t

    val get : ('a, 'a) t
    val put : 'a -> (unit, 'a) t

    val run : ('a, 'b) t -> 'b -> 'a * 'b
    val eval : ('a, 'b) t -> 'b -> 'a
    val exec : ('a, 'b) t -> 'b -> 'b
end = struct
    type ('a, 'b) t = State of ('b -> 'a * 'b)

    let value (State s) = s
    let bind m f = State (fun s -> let a, s' = (value m) s in ((value (f a)) s'))
    let (>>=) = bind
    let return x = State (fun s -> (x, s))

    let get = State (fun s -> (s, s))
    let put s = State (fun _ -> ((), s))

    let (&&&) f1 f2 (t1, t2) = get >>= fun s ->
        f1 t1 >>= fun r1 ->
        put s >>= fun () ->
        f2 t2 >>= fun r2 ->
        put s >>= fun () ->
        return (r1, r2)

    let rec sequence' = function
        | [] -> return []
        | hd :: rest -> 
            get >>= fun s ->
            hd >>= fun hd ->
            put s >>= fun () ->
            sequence' rest >>= fun rest ->
            put s >>= fun () ->
            return (hd :: rest)

    let run (State m) a = m a
    let eval m a = fst (run m a)
    let exec m a = snd (run m a)
end

module AST = struct

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
end

module Type = struct
    type t = Atom of string
           | Fun of t * t

    let resolve_atomic t = match t.[0] with
        | 'x' | 'y' | 'z' -> Some (Atom "Entity")
        | 'e' -> Some (Atom "Event")
        | _ -> None
end

type variable = int

module Term : sig
    type t

    val mkTrue : unit -> t
    val mkFalse : unit -> t
    val mkVar : unit -> t
    val mkConst : string -> t
    val mkNot : t -> t
    val mkEqual : t -> t -> t
    val mkAnd : t -> t -> t
    val mkOr : t -> t -> t
    val mkImp : t -> t -> t
    val mkApp : t -> t list -> t
    val mkLambda : ?ty:Type.t -> t -> t
    val mkExists : ?ty:Type.t -> t -> t
    val mkForall : ?ty:Type.t -> t -> t

    val from_AST : AST.t -> t
    val show : t -> string
    val shift_indices : variable -> variable -> t -> t
    val subst : t -> variable -> t -> t
    val beta_reduce : t -> t
    val normalize : t -> t
end = struct

    type t = True
           | False
           | Var of variable
           | Const of string
           | Not of t
           | Equal of t * t
           | And of t * t
           | Or of t * t
           | Imp of t * t
           | App of t * t
           | Lambda of (t * Type.t option)
           | Exists of (t * Type.t option)
           | Forall of (t * Type.t option)

    let mkTrue () = True
    let mkFalse () = False
    let mkVar () = Var 0
    let mkConst s = Const s
    let mkNot t = Not t
    let mkEqual t1 t2 = Equal (t1, t2)
    let mkAnd t1 t2 = And (t1, t2)
    let mkOr t1 t2 = Or (t1, t2)
    let mkImp t1 t2 = Imp (t1, t2)
    let mkApp f xs = List.fold_left (fun f x -> App (f, x)) f xs
    let mkLambda ?ty t = Lambda (t, ty)
    let mkExists ?ty t = Exists (t, ty)
    let mkForall ?ty t = Forall (t, ty)

    let get_args = 
        let rec aux args = function
        | App (xs, x) -> aux (x :: args) xs
        | f -> (f, args)
        in function
        | App (_, _) as f -> aux [] f
        | _ -> invalid_arg ("get_args expects an App as input")

    let get_lambda_vars = 
        let rec aux vars = function
        | Lambda (t, ty) -> aux (ty :: vars) t
        | t -> (t, vars) (* vars are in reverse order (lower one comes first) *)
        in function
        | Lambda (_, _) as f -> aux [] f
        | _ -> invalid_arg ("get_lambda_vars expects an Lambda as input")

    let put_cons i = StateM.(get >>=
        fun lst -> put (i :: lst))

    let from_AST t =
        let rec f = StateM.(function
        | AST.True  -> return True
        | AST.False -> return False
        | AST.Term s -> do_;
            lst <-- get;
            (try let i = List.index s lst in
                return @@ Var i
            with Not_found ->
                return @@ Const s)
        | AST.Not t -> do_;
            t' <-- f t;
            return @@ Not t'
        | AST.Equal (t1, t2) -> do_;
            (t1', t2') <-- (f &&& f) (t1, t2);
            return @@ Equal (t1', t2')
        | AST.And (t1, t2) -> do_;
            (t1', t2') <-- (f &&& f) (t1, t2);
            return @@ And (t1', t2')
        | AST.Or (t1, t2) -> do_;
            (t1', t2') <-- (f &&& f) (t1, t2);
            return @@ Or (t1', t2')
        | AST.Imp (t1, t2) -> do_;
            (t1', t2') <-- (f &&& f) (t1, t2);
            return @@ Imp (t1', t2')
        | AST.App (t1, t2) -> do_;
            (t1', t2') <-- (f &&& f) (t1, t2);
            return @@ App (t1', t2')
        | AST.Lambda (v, t) -> do_;
            put_cons v;
            t' <-- f t;
            return @@ Lambda (t', Type.resolve_atomic v)
        | AST.Exists (v, t) -> do_;
            put_cons v;
            t' <-- f t;
            return @@ Exists (t', Type.resolve_atomic v)
        | AST.Forall (v, t) -> do_;
            put_cons v;
            t' <-- f t;
            return @@ Forall (t', Type.resolve_atomic v)
        )
        in StateM.eval (f t) []

    let show t =
        let gen_varname = let _idx = ref (0, 0, 0) in
            (fun ty ->
                let (e, x, u) = !_idx in
                let res, idx = match ty with
                    | Some (Type.Atom "Event")  -> (!%"e%d" e), (e+1, x, u)
                    | Some (Type.Atom "Entity") -> (!%"x%d" x), (e, x+1, u)
                    | _ -> (!%"u%d" u), (e, x, u+1) in
                _idx := idx; res) in
        let rec f = StateM.(function
        | True  -> return "True"
        | False -> return "False"
        | Var v -> do_;
            lst <-- get;
            return (List.nth lst v)
        | Const s -> return s
        | Not t -> do_;
            t' <-- f t;
            return @@ !%"~(%s)" t'
        | Equal (t1, t2) -> do_;
            (t1', t2') <-- (f &&& f) (t1, t2);
            return @@ !%"%s = %s" t1' t2'
        | And (t1, t2) -> do_;
            (t1', t2') <-- (f &&& f) (t1, t2);
            return @@ !%"(%s /\\ %s)" t1' t2'
        | Or (t1, t2) -> do_;
            (t1', t2') <-- (f &&& f) (t1, t2);
            return @@ !%"(%s \\/ %s)" t1' t2'
        | Imp (t1, t2) -> do_;
            (t1', t2') <-- (f &&& f) (t1, t2);
            return @@ !%"%s -> %s" t1' t2'
        | App _ as t ->
            let (func, args) = get_args t in
            let fmt = match func with
                | Const _ | Var _ -> format_of_string "%s(%s)"
                | Lambda _ -> format_of_string "(%s)(%s)"
                | _ -> format_of_string "%s (%s)" in
            let g args = sequence' @@ List.map f args in do_;
            (func, args) <-- (f &&& g) (func, args);
            return @@ !%fmt func (String.concat ", " args)
        | Lambda _ as t ->
            let t, vars = get_lambda_vars t in
            let vars = List.rev_map gen_varname vars in do_;
            st <-- get;
            put ((List.rev vars) @ st);
            body <-- f t;
            return @@ !%"\\%s. %s" (String.concat " " vars) body
        | Exists (t, ty) ->
            let var = gen_varname ty in do_;
            put_cons var;
            body <-- f t;
            return @@ !%"exists %s. (%s)" var body
        | Forall (t, ty) ->
            let var = gen_varname ty in do_;
            put_cons var;
            body <-- f t;
            return @@ !%"forall %s. (%s)" var body
        )
        in StateM.eval (f t) []

    let rec shift_indices d i = function
        | Var j          -> if j >= i
                                then Var (j + d)
                                else Var j
        | Not t          -> Not (shift_indices d i t)
        | Equal (t1, t2) -> Equal (shift_indices d i t1, shift_indices d i t2)
        | And (t1, t2)   -> And (shift_indices d i t1, shift_indices d i t2)
        | Or (t1, t2)    -> Or (shift_indices d i t1, shift_indices d i t2)
        | Imp (t1, t2)   -> Imp (shift_indices d i t1, shift_indices d i t2)
        | App (t1, t2)   -> App (shift_indices d i t1, shift_indices d i t2)
        | Lambda (t, ty) -> Lambda (shift_indices d (i + 1) t, ty)
        | Exists (t, ty) -> Exists (shift_indices d (i + 1) t, ty)
        | Forall (t, ty) -> Forall (shift_indices d (i + 1) t, ty)
        | t -> t

    let rec subst l i = function
        | Var j          -> if j = i
                                then l
                                else Var j
        | Not t          -> Not (subst l i t)
        | Equal (t1, t2) -> Equal (subst l i t1, subst l i t2)
        | And (t1, t2)   -> And (subst l i t1, subst l i t2)
        | Or (t1, t2)    -> Or (subst l i t1, subst l i t2)
        | Imp (t1, t2)   -> Imp (subst l i t1, subst l i t2)
        | App (t1, t2)   -> App (subst l i t1, subst l i t2)
        | Lambda (t, ty) -> Lambda (subst (shift_indices 1 0 l) (i + 1) t, ty)
        | Exists (t, ty) -> Exists (subst (shift_indices 1 0 l) (i + 1) t, ty)
        | Forall (t, ty) -> Forall (subst (shift_indices 1 0 l) (i + 1) t, ty)
        | t -> t

    let rec beta_reduce = function
        | Not t          -> Not (beta_reduce t)
        | Equal (t1, t2) -> Equal (beta_reduce t1, beta_reduce t2)
        | And (t1, t2)   -> And (beta_reduce t1, beta_reduce t2)
        | Or (t1, t2)    -> Or (beta_reduce t1, beta_reduce t2)
        | Imp (t1, t2)   -> Imp (beta_reduce t1, beta_reduce t2)
        | App (t1, t2)   -> begin match beta_reduce t1 with
            | Lambda (t, ty) -> beta_reduce (shift_indices (-1) 0 (subst (shift_indices 1 0 t2) 0 t))
            | t -> App (t, beta_reduce t2)
        end
        | Lambda (t, ty) -> Lambda (beta_reduce t, ty)
        | Exists (t, ty) -> Exists (beta_reduce t, ty)
        | Forall (t, ty) -> Forall (beta_reduce t, ty)
        | t -> t

    let rec normalize = function
        | Not t          -> Not (normalize t)
        | Equal (t1, t2) -> Equal (normalize t1, normalize t2)
        | And (True, t) | And (t, True) -> normalize t
        | And (t1, t2)   -> And (normalize t1, normalize t2)
        | Or (False, t) | Or (t, False) -> normalize t
        | Or (t1, t2)    -> Or (normalize t1, normalize t2)
        | Imp (True, t)  -> normalize t
        | Imp (t1, t2)   -> Imp (normalize t1, normalize t2)
        | App (t1, t2)   -> App (normalize t1, normalize t2)
        | Lambda (t, ty) -> Lambda (normalize t, ty)
        | Exists (t, ty) -> Exists (normalize t, ty)
        | Forall (t, ty) -> Forall (normalize t, ty)
        | t -> t
end


