open ID

module type Evaluable =
  sig
    type t
    type var
    type valuation
    type value
    type rename_map
    val (==) : t -> t -> bool
    val of_string : string -> t
    val to_string : t -> string
    val compare : t -> t -> int
    val vars : t -> var list
    val eval : t -> valuation -> value
    val to_z3 : Z3.context -> t -> Z3.Expr.expr
    val rename : rename_map -> t -> t
  end

module type EvaluableFunctor =
  functor (Var : ID) -> Evaluable with type var = Var.t
