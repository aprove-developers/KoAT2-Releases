open Batteries
module Inner : PolyTypes.Polynomial
module Outer : PolyTypes.Polynomial
include PolyTypes.Polynomial
val flatten : Outer.t -> Inner.t