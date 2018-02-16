
exception Parse_error of string


module Type :
sig
    type t = Atom of string
           | Fun of t * t

    val resolve_atomic : string -> t option
end

type variable = int

module Term :
sig
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

    val from_AST : Ast.t -> t
    val show : t -> string
    val shift_indices : variable -> variable -> t -> t
    val subst : t -> variable -> t -> t
    val beta_reduce : t -> t
    val normalize : t -> t
end


val parse : string -> Term.t

val parse_option : string -> Term.t option
