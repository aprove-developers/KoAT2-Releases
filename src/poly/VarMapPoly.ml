open! OurBase
include MakeMapCreators1 (Var)
open Polynomials

type map_type = (Var.t, Polynomial.t, key_comparator_witness) Map.t

let equal_map_type = Map.equal Polynomial.equal
let compare_map_type = Map.compare_direct Polynomial.compare
let map_type_of_sexp = Map.m__t_of_sexp (module Var) Polynomial.t_of_sexp
let sexp_of_map_type = Map.sexp_of_m__t (module Var) Polynomial.sexp_of_t

let to_string map =
  Util.sequence_to_string ~f:(fun (v, p) -> "(" ^ Var.to_string v ^ "," ^ Polynomial.to_string p ^ ")")
  @@ Map.to_sequence map
