open Batteries

include Hashtbl.Make(Program.TransitionGraph.V)

type table = (Polynomials.ParameterPolynomial.t) t