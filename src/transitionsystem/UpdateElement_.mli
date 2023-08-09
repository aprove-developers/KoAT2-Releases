open OurBase
open Polynomials

module UpdateValue : sig
  type t = Var of Var.t | Dist of ProbabilityDistribution.t [@@deriving eq, ord]

  val sexp_of_t : t -> OurBase.Sexp.t
  val to_string : ?pretty:bool -> ?to_file:bool -> t -> string
  val rename : RenameMap.t -> t -> t
  val of_var : Var.t -> t
  val vars : t -> VarSet.t

  val is_dist : t -> bool
  (** Is this a distribution update? *)

  val get_var : t -> Var.t option
  (** Get the variable in case of a deterministic update *)

  val is_integral : t -> bool
  val as_guard : t -> Var.t -> Guard.t

  (* Get a polynomial representing the expected value *)
  val exp_value_poly : t -> RealPolynomial.t

  (* Get a polynomial representing the corresponding non-central moment, i.e. E(d^i) *)
  val moment_poly : t -> int -> RealPolynomial.t

  include Comparator.S with type t := t
end

include module type of PolynomialOverIndeterminate (UpdateValue) (OurInt)

val of_poly : Polynomial.t -> t
(** Create an UpdateElement from a Polynomial *)

val of_dist : ProbabilityDistribution.t -> t

(* {i update_guard ue v} expresses the effects of the updatelement ue as guard, i.e., the returned guard enforces variable {i v} to lie in the support of {i ue} *)
val as_guard : t -> Var.t -> Guard.t
val exp_value_poly : t -> RealPolynomial.t
val exp_value_abs_bound : t -> Bounds.RealBound.t
val moment_poly : t -> int -> RealPolynomial.t

(* try to convert the UpdateElement to a Polynomial *)
val to_polynomial : t -> Polynomial.t option

(* Restores legacy semantics for updates with distributions, i.e. updates of the form X->UNIFORM(0,1) are interpreted as X->X+UNIFORM(0,1).
   The variable corresponds to the update variable that will be added to the distribution *)
val restore_legacy_distribution_update_semantics : Var.t -> t -> t

val admissibility_constraint : t -> Guard.t
(** Returns a constraint that encodes the admissibility of distribution updates. e.g. for {i UNIFORM(p1,p2)} we require {i p1<=p2} *)

val as_linear_guard : Guard.t -> t -> Var.t -> Guard.t
(** Express the UpdateElement as a linear guard in the new variable.
    Can make use of the information present in the supplied guard. *)
