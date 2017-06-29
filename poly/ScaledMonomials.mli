type scaled_mon = Scaled of (Big_int.big_int * Monomials.monomial)
val mk_scaled_mon_from_mon :
  Big_int.big_int -> Monomials.monomial -> scaled_mon
val to_z3 : Z3.context -> scaled_mon -> Z3.Expr.expr
val get_coeff : scaled_mon -> Big_int.big_int
val get_monom : scaled_mon -> Monomials.monomial
val get_degree : scaled_mon -> int
val simplify : scaled_mon -> scaled_mon
val to_string_simplified : scaled_mon -> string
val to_string : scaled_mon -> string
val equal : scaled_mon -> scaled_mon -> bool
val rename_scaled_mon : string Mapping.VarMap.t -> scaled_mon -> scaled_mon
val instantiate_with_big_int :
  Big_int.big_int Mapping.VarMap.t -> scaled_mon -> Big_int.big_int
val mult_with_const : Big_int.big_int -> scaled_mon -> scaled_mon
val mult : scaled_mon -> scaled_mon -> scaled_mon
