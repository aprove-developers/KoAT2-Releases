open Batteries
open Polynomials

module UpdateValue : sig
  type t = Var of Var.t
         | Dist of ProbabilityDistribution.t [@@deriving eq,ord]

  val to_string: ?pretty:bool -> ?to_file:bool -> t -> string

  val rename: RenameMap.t -> t -> t

  val of_var: Var.t -> t

  val vars: t -> VarSet.t

  (** Is this a distribution update? *)
  val is_dist: t -> bool

  (** Get the variable in case of a deterministic update *)
  val get_var: t -> Var.t option

  val is_integral: t -> bool

  val as_guard: t -> Var.t -> Guard.t
end

include module type of PolynomialOverIndeterminate(UpdateValue)(OurInt)

(** Create an UpdateElement from a Polynomial *)
val of_poly: Polynomial.t -> t

val of_dist: ProbabilityDistribution.t -> t

(* {i update_guard ue v} expresses the effects of the updatelement ue as guard, i.e., the returned guard enforces variable {i v} to lie in the support of {i ue} *)
val as_guard: t -> Var.t -> Guard.t

val exp_value_poly: t -> RealPolynomial.t

val moment_poly: t -> int -> RealPolynomial.t

(* try to convert the UpdateElement to a Polynomial *)
val to_polynomial: t -> Polynomial.t option

(* Restores legacy semantics for updates with distributions, i.e. updates of the form X->UNIFORM(0,1) are interpreted as X->X+UNIFORM(0,1).
   The variable corresponds to the update variable that will be added to the distribution   *)
val restore_legacy_distribution_update_semantics: Var.t -> t -> t

(** Returns a constraint that encodes the admissibility of distribution updates. e.g. for {i UNIFORM(p1,p2)} we require {i p1<=p2} *)
val admissibility_constraint: t -> Guard.t

(** Express the UpdateElement as a linear guard in the new variable.
    Can make use of the information present in the supplied guard. *)
val as_linear_guard: Guard.t -> t -> Var.t -> Guard.t
