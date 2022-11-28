open Batteries
open Polynomials

type t = Binomial of Polynomial.t * OurFloat.t
       | Geometric of OurFloat.t
       | Hypergeometric of OurInt.t * Polynomial.t * Polynomial.t
       | Uniform of Polynomial.t * Polynomial.t [@@deriving eq,ord]

val to_string: ?pretty:bool -> ?to_file:bool -> t -> string

val equal: t -> t -> bool

val rename: RenameMap.t -> t -> t

val vars: t -> VarSet.t

(** {i as_guard d v } constructs a guard that requires v to be in the support of d *)
val as_guard: t -> Var.t -> Guard.t

(** overapproximates the expected value of the distribution as a poly *)
val exp_value_poly: t -> RealPolynomial.t

(** {i moment_poly d i} is the {i i}-th non-central moment of distribution {i d}, i.e., {i E(d^i)} *)
val moment_poly: t -> int -> RealPolynomial.t

(** Returns a constraint that encodes the admissibility of distribution updates. e.g. for {i UNIFORM(p1,p2)} we require {i p1<=p2} *)
val admissibility_constraint: t -> Guard.t
