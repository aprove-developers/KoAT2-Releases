type scaled_mon
type value = Big_int.big_int
val mk_scaled_mon_from_mon : value -> Monomials.t -> scaled_mon
val to_z3 : Z3.context -> scaled_mon -> Z3.Expr.expr
val get_coeff : scaled_mon -> value
val get_monom : scaled_mon -> Monomials.t
val get_degree : scaled_mon -> int
val simplify : scaled_mon -> scaled_mon
val to_string_simplified : scaled_mon -> string
val to_string : scaled_mon -> string
val equal : scaled_mon -> scaled_mon -> bool
val rename_scaled_mon : string Mapping.VarMap.t -> scaled_mon -> scaled_mon
val eval : value Mapping.VarMap.t -> scaled_mon -> value
val mult_with_const : value -> scaled_mon -> scaled_mon
val mult : scaled_mon -> scaled_mon -> scaled_mon
