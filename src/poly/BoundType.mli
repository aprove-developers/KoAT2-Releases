open Batteries
open Polynomials

(** A MinMaxPolynomial is a polynomial which allows the usage of min and max functions  *)

module Make_BoundOver :
  functor (Num : PolyTypes.OurNumber) -> functor
          (Poly :
             sig
               include PolyTypes.Polynomial with type value = Num.t
                                             and type valuation = Valuation.Make(Num).t
                                             and type monomial = Monomials.Make(Num).t
               val max_of_occurring_constants : t -> Num.t
             end ) ->
    sig

      type t

      include PolyTypes.Evaluable with type t := t with type value = Num.t
      include PolyTypes.Math with type t := t
      include PolyTypes.PartialOrder with type t := t

      (** Following methods are convenience methods for the creation of polynomials. *)

      val of_poly : Poly.t -> t
      val of_constant : value -> t
      val of_int : int -> t
      val to_int : t -> int
      val of_var : Var.t -> t
      val of_var_string : string -> t

      val min : t -> t -> t
      val max : t -> t -> t

      (** Returns a bound representing the minimum of all the values.
          Raises an exception, if the enum is empty.
          Use the function infinity for those cases. *)
      val minimum : t Enum.t -> t
      (** Returns a bound representing the maximum of all the values.
          Raises an exception, if the enum is empty.
          Use the function minus_infinity for those cases. *)
      val maximum : t Enum.t -> t

      val infinity : t
      val minus_infinity : t
      val exp : value -> t -> t
      val abs : t -> t

      val abs_bound : ([`Lower | `Upper] -> t) -> t

      val max_of_occurring_constants : t -> Num.t

      val is_infinity : t -> bool

      val is_minus_infinity : t -> bool

      val to_string : t -> string

      val show : ?complexity:bool -> t -> string

      (** Functions to classify the quality of the bound *)


      (** Following methods can be used to classify the type of the polynomial. *)

      (** Substitutes every occurrence of the variable in the polynomial by the replacement polynomial.
              Ignores naming equalities. *)
      val substitute : Var.t -> replacement:t -> t -> t

      (** Substitutes every occurrence of the variables in the polynomial by the corresponding replacement polynomial.
              Leaves all variables unchanged which are not in the replacement map.  *)
      val substitute_all : t Map.Make(Var).t -> t -> t

      (** Substitutes every occurrence of the variables in the polynomial by the corresponding replacement polynomial. *)
      val substitute_f : (Var.t -> t) -> t -> t

      val appr_substitution : [ `Lower | `Upper ] -> lower:(Var.t -> t) -> higher:(Var.t -> t) -> t -> t

      (** Similar to appr_substitution but operates on bounds of absolute values and only substitutes variables with substition kind Probabilistic*)
      val appr_substition_abs_probabilistic : (Var.t -> t) -> t -> t

      (** Similar to appr_substitution but operates on bounds of absolute values and only substitutes variables with substition kind NonProbabilistic*)
      val appr_substition_abs_nonprobabilistic : (Var.t -> t) -> t -> t

      (** Similar to appr_substitution but operates on bounds of absolute values *)
      val appr_substition_abs_all : (Var.t -> t) -> t -> t

      (** Similar to appr_substitution_abs_all but substitutes only one variable *)
      val appr_substitute_abs : Var.t -> t -> t -> t

      (* Similar to appr_substitution_abs_all but choses the substitution function according the the variable kind *)
      val appr_substitution_probabilistic_and_nonprobabilistic: probabilistic:(Var.t -> t) -> nonprobabilistic:(Var.t -> t) -> t -> t

      (** Replaces all arithmetical operations by new constructors. *)
      val fold : const:(value -> 'b) ->
                 var:(Var.t -> 'b) ->
                 neg:('b -> 'b) ->
                 plus:('b -> 'b -> 'b) ->
                 times:('b -> 'b -> 'b) ->
                 exp:(value -> 'b -> 'b) ->
                 max:('b -> 'b -> 'b) ->
                 abs:('b -> 'b) ->
                 inf:'b ->
                 t -> 'b

      type complexity =
        | Inf
        | Polynomial of int
        | Exponential of int

      val equal_complexity : complexity -> complexity -> bool

      val show_complexity : complexity -> string

      val show_complexity_termcomp : complexity -> string

      (** Returns an overapproximation of the asymptotic complexity of the given bound. *)
      val asymptotic_complexity : t -> complexity

      (** Returns true iff the asymptotic complexity is n^1. *)
      val is_linear : t -> bool

      val set_linear_vars_to_probabilistic_and_rest_to_nonprobabilistic : t -> t

      (** When bound b is linear in variable v is_linear_in_var v b returns true.  Note that max(v,v') is neither linear in v nor v'. *)
      val is_linear_in_var : Var.t -> t -> bool

      (** Needed for Atomizable but not yet implemented. *)
      val coeff_of_var : Var.t -> t -> value

      (** Needed for Atomizable but not yet implemented. *)
      val of_coeff_list : value list -> Var.t list -> t

      (** returns the constant of a bound *)
      val get_constant : t -> value

      (** if the bound is a constant it retuns this constant *)
      val get_constant_option: t -> value option
    end
