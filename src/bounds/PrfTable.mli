open Batteries
open Polynomials
open Program.Types
   
include module type of Hashtbl.Make(TransitionGraph.V)

type parameter_table = ParameterPolynomial.t t

type polynomial_table = Polynomial.t t

val to_string_poly : polynomial_table -> string

val to_string_parapoly : parameter_table -> string
