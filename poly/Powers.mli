type pow = 
    {
        var : Variables.t; 
        n : int
    }
type value = Big_int.big_int
val to_string : pow -> string
val to_z3 : Z3.context -> pow -> Z3.Expr.expr
val mk_pow_from_var : Variables.t -> int -> pow
val mk_var : string -> pow
val get_variable : pow -> Variables.t
val get_degree : pow -> int
val equal : pow -> pow -> bool
val rename_power : string Mapping.VarMap.t -> pow -> pow
val eval : value Mapping.VarMap.t -> pow -> value
