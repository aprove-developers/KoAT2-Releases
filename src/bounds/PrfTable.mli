open Batteries

include module type of Hashtbl.Make(Program.TransitionGraph.V)

type parameter_table = (Polynomials.ParameterPolynomial.t) t

type polynomial_table = (Polynomials.Polynomial.t) t

val to_string_poly : polynomial_table -> string

val to_string_parapoly : parameter_table -> string