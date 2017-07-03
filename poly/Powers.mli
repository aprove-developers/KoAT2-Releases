type t = 
    {
        var : Variables.t; 
        n : int
    }
type value = Big_int.big_int
val to_string : t -> string
val to_z3 : Z3.context -> t -> Z3.Expr.expr
val mk_pow_from_var : Variables.t -> int -> t
val mk_var : string -> t
val get_variable : t -> Variables.t
val get_degree : t -> int
val equal : t -> t -> bool
val rename_power : string Mapping.VarMap.t -> t -> t
val eval : value Mapping.VarMap.t -> t -> value
