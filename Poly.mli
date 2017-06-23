module VarMap :
sig
  type key = String.t
  type 'a t = 'a Map.Make(String).t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : key -> 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val singleton : key -> 'a -> 'a t
  val remove : key -> 'a t -> 'a t
  val merge :
    (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val for_all : (key -> 'a -> bool) -> 'a t -> bool
  val exists : (key -> 'a -> bool) -> 'a t -> bool
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val cardinal : 'a t -> int
  val bindings : 'a t -> (key * 'a) list
  val min_binding : 'a t -> key * 'a
  val max_binding : 'a t -> key * 'a
  val choose : 'a t -> key * 'a
  val split : key -> 'a t -> 'a t * 'a option * 'a t
  val find : key -> 'a t -> 'a
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
end
     
module Variable :
sig
  type variable = string
  val mk_var : string -> string
  val to_string : variable -> variable
  val varlist_to_string : variable list -> string
  val equal : variable -> variable -> bool
  val to_z3 : Z3.context -> variable -> Z3.Expr.expr
  val get_new_var_name : string VarMap.t -> variable -> variable
  val instantiate_with_big_int :
    Big_int.big_int VarMap.t -> variable -> Big_int.big_int
end
     
module Powers :
sig
  type pow = Pow of Variables.variable * int
  val to_string : pow -> Variables.variable
  val to_z3 : Z3.context -> pow -> Z3.Expr.expr
  val mk_pow_from_var : Variables.variable -> int -> pow
  val get_variable : pow -> Variables.variable
  val get_degree : pow -> int
  val equal : pow -> pow -> bool
  val rename_power : string VarMap.t -> pow -> pow
  val instantiate_with_big_int :
    Big_int.big_int VarMap.t -> pow -> Big_int.big_int
end
     
module Monomials :
sig
  type monomial = Powers.pow list
  val to_z3 : Z3.context -> monomial -> Z3.Expr.expr
  val mk_mon : (Variables.variable * int) list -> Powers.pow list
  val get_variables : monomial -> Variables.variable list
  val get_degree : monomial -> int
  val get_degree_variable : Variables.variable -> monomial -> int
  val delete_var : Variables.variable -> monomial -> Powers.pow list
  val simplify : monomial -> Powers.pow list
  val to_string_simplified : monomial -> string
  val to_string : monomial -> string
  val equal_simplified : monomial -> monomial -> bool
  val equal : monomial -> monomial -> bool
  val is_univariate_linear_monomial : monomial -> bool
  val rename_monomial : string VarMap.t -> monomial -> Powers.pow list
  val mult : monomial -> monomial -> Powers.pow list
  val instantiate_with_big_int :
    Big_int.big_int VarMap.t -> monomial -> Big_int.big_int
end
     
module ScaledMonomials :
sig
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
  val rename_scaled_mon : string VarMap.t -> scaled_mon -> scaled_mon
  val instantiate_with_big_int :
    Big_int.big_int VarMap.t -> scaled_mon -> Big_int.big_int
  val mult_with_const : Big_int.big_int -> scaled_mon -> scaled_mon
  val mult : scaled_mon -> scaled_mon -> scaled_mon
end
     
module Polynomials :
sig
  type polynomial = ScaledMonomials.scaled_mon list
  val get_degree : polynomial -> int
  val to_z3 : Z3.context -> polynomial -> Z3.Expr.expr
  val get_coeff : Monomials.monomial -> polynomial -> Big_int.big_int
  val delete_monomial :
    Monomials.monomial -> polynomial -> ScaledMonomials.scaled_mon list
  val simplify_partial_simplified :
    polynomial -> ScaledMonomials.scaled_mon list
  val simplify : polynomial -> ScaledMonomials.scaled_mon list
  val to_string_simplified : polynomial -> string
  val to_string : polynomial -> string
  val equal_simplified : polynomial -> polynomial -> bool
  val equal : polynomial -> polynomial -> bool
  val get_monomials : polynomial -> Monomials.monomial list
  val from_var : Variables.variable -> ScaledMonomials.scaled_mon list
  val zero : 'a list
  val one : ScaledMonomials.scaled_mon list
  val get_constant : polynomial -> Big_int.big_int
  val from_constant : Big_int.big_int -> ScaledMonomials.scaled_mon list
  val get_variables : polynomial -> Variables.variable list
  val is_var : polynomial -> bool
  val is_var_plus_constant : polynomial -> bool
  val is_sum_of_vars_plus_constant : polynomial -> bool
  val is_univariate_and_linear : polynomial -> bool
  val is_const : polynomial -> bool
  val is_linear : polynomial -> bool
  val rename_vars :
    string VarMap.t -> polynomial -> ScaledMonomials.scaled_mon list
  val mult_with_const :
    Big_int.big_int -> polynomial -> ScaledMonomials.scaled_mon list
  val negate : polynomial -> ScaledMonomials.scaled_mon list
  val add : polynomial -> polynomial -> ScaledMonomials.scaled_mon list
  val add_list : polynomial list -> ScaledMonomials.scaled_mon list
  val subtract :
    polynomial -> polynomial -> ScaledMonomials.scaled_mon list
  val mult : polynomial -> polynomial -> polynomial
  val pow_poly : polynomial -> int -> polynomial
  val instantiate_with_big_int :
    Big_int.big_int VarMap.t -> polynomial -> Big_int.big_int
end
