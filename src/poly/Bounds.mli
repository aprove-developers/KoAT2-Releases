open! OurBase

module Make (Num : PolyTypes.OurNumber) : sig
  type t
  type bound

  type complexity =
    | Inf  (** Bound is infinite. *)
    | Polynomial of int  (** Bound is in asymptotic class O(n^i) *)
    | Exponential of int

  val fold :
    const:(Num.t -> 'b) ->
    var:(Var.t -> 'b) ->
    plus:('b -> 'b -> 'b) ->
    times:('b -> 'b -> 'b) ->
    exp:(Num.t -> 'b -> 'b) ->
    inf:'b ->
    t ->
    'b
  (** Replaces all arithmetical operations by new constructors. *)

  val fold_bound :
    const:(Num.t -> 'b) ->
    var:(Var.t -> 'b) ->
    plus:('b -> 'b -> 'b) ->
    times:('b -> 'b -> 'b) ->
    exp:(Num.t -> 'b -> 'b) ->
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

  val to_intbound : t -> Bound.t
  (** Ceil Floats to Ints *)
end

module BinaryBound : sig
  type t = Finite | Infinite

  include BoundType.Bound with type t := t
end
