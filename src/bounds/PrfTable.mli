open Batteries
open Polynomials
open Program.Types
   
include module type of Hashtbl.Make(Location)

val to_string_poly : Polynomial.t t -> string

val to_string_parapoly : ParameterPolynomial.t t -> string
