open! OurBase
include module type of MakeMapCreators1 (Var)

type map_type = (Var.t, Polynomials.Polynomial.t, key_comparator_witness) Map.t

val equal_map_type : map_type -> map_type -> bool
val compare_map_type : map_type -> map_type -> int
val map_type_of_sexp : Sexp.t -> map_type
val sexp_of_map_type : map_type -> Sexp.t
val to_string : map_type -> string
