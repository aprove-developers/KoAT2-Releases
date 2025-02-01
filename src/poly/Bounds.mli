open! OurBase

module Make (Num : PolyTypes.OurNumber) : sig
  type t
  type bound

  type complexity =
    | LogarithmicPolynomial of OurRational.t * OurRational.t
        (** Bound is in asymptotic class O(log(n)^i * n^j) *)
    | Exponential of int
    | Inf  (** Bound is infinite. *)

  val fold :
    const:(Num.t -> 'b) ->
    var:(Var.t -> 'b) ->
    plus:('b -> 'b -> 'b) ->
    times:('b -> 'b -> 'b) ->
    exp:('b -> 'b -> 'b) ->
    log:(Var.t -> 'b) ->
    inf:'b ->
    t ->
    'b
  (** Replaces all arithmetical operations by new constructors. *)

  val fold_bound :
    const:(Num.t -> 'b) ->
    var:(Var.t -> 'b) ->
    plus:('b -> 'b -> 'b) ->
    times:('b -> 'b -> 'b) ->
    exp:('b -> 'b -> 'b) ->
    log:(Var.t -> 'b) ->
    bound ->
    'b
  (** Replaces all arithmetical operations by new constructors in finite bounds. *)

  include
    BoundType.Bound
      with type value = Num.t
       and type polynomial = Polynomials.PolynomialOver(Num).t
       and type complexity := complexity
       and type bound := bound
       and type t := t
end

module Bound : module type of Make (OurInt)

module RationalBound : sig
  include module type of Make (OurRational)

  val of_intbound : Bound.t -> t
  val of_intpoly : Polynomials.Polynomial.t -> t

  val of_overapprox_laurentpoly : Polynomials.RationalLaurentPolynomial.t -> t
  (** Overapproximates variables with negative exponents with 1 *)

  val to_intbound : t -> Bound.t
  (** Ceil Floats to Ints *)

  val sqrt : t -> t
  (** Computes the square root *)

  val mth_root : OurInt.t -> t -> t
  (** [mth_root m b] Computes the [b]m-th square root of [b], i.e., b^(1/m)*)
end

module BinaryBound : sig
  type t = Finite | Infinite

  include BoundType.Bound with type t := t
end
