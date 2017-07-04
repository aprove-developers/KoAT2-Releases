type t
type value = Big_int.big_int
val mk_scaled_mon_from_mon : value -> Monomials.t -> t
val to_z3 : Z3.context -> t -> Z3.Expr.expr
val get_coeff : t -> value
val get_monom : t -> Monomials.t
val get_degree : t -> int
val simplify : t -> t
val to_string_simplified : t -> string
val to_string : t -> string
val equal : t -> t -> bool
val rename_scaled_mon : string Mapping.VarMap.t -> t -> t
val eval : value Mapping.VarMap.t -> t -> value
val mult_with_const : value -> t -> t
val mult : t -> t -> t
