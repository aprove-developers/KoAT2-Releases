open! OurBase
(** Provides default implementation of a monomial.
    When used in Laurent polynomials, the exponents may be negative. *)

type ('indeterminate, 'indeterminate_cmp_wit) generic_monomial
(** generic type of generated monomials *)

type generic_var_monomial = (Var.t, Var.comparator_witness) generic_monomial
(** type of generated monomials with variables as indeterminates *)

type 'indeterminate_cmp_wit generic_monomial_comparator_witness
(** generic monomial comparator witness*)

type generic_var_monomial_comparator_witness
(** generic monomial comparator witness for monomials with variables as indeterminates *)

module MakeOverIndeterminate (I : PolyTypes.Indeterminate) (Value : PolyTypes.Ring) :
  PolyTypes.Monomial
    with type value = Value.t
     and type valuation = Valuation.MakeOverIndeterminate(I)(Value).t
     and type indeterminate = I.t
     and type t = (I.t, I.comparator_witness) generic_monomial
     and type comparator_witness = I.comparator_witness generic_monomial_comparator_witness

(** Constructs a default monomial using a list of pairs of variables and their exponents.
    When used in Laurent polynomials, the exponents may be negative. *)
module Make (Value : PolyTypes.Ring) :
  PolyTypes.Monomial
    with type value = Value.t
     and type valuation = Valuation.MakeOverIndeterminate(VarIndeterminate)(Value).t
     and type indeterminate = Var.t
     and type t = MakeOverIndeterminate(VarIndeterminate)(Value).t
