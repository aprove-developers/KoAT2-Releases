open Polynomials

module TransitionGraphWeight(Value : PolyTypes.Ring) = struct
    type t = Value.t
    type edge = TransitionGraph_.E.t
    let weight (x : edge) = Value.one
    let compare x y = 0
    let add x y = Value.add x y
    let zero = Value.zero
  end
