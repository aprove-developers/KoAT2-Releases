open OurBase
open Bounds
open Polynomials
open ProgramModules
open ProgramTypes

module Automorphism = struct
  type t = { poly : RationalPolynomial.t var_map; inv_poly : RationalPolynomial.t var_map }

  let to_string t =
    "Automorphism:\n"
    ^ Map.fold
        ~f:(fun ~key ~data str -> str ^ Var.to_string key ^ " -> " ^ RationalPolynomial.to_string data ^ "\n")
        t.poly ~init:""
    ^ "Inverse: \n"
    ^ Map.fold
        ~f:(fun ~key ~data str -> str ^ Var.to_string key ^ " -> " ^ RationalPolynomial.to_string data ^ "\n")
        t.inv_poly ~init:""


  let identity = { poly = Map.empty (module Var); inv_poly = Map.empty (module Var) }

  let apply_to_bound bound = function
    | None -> bound
    | Some t ->
        List.fold_right
          ~f:(fun var bound ->
            let bound_of_inv_poly =
              Bound.of_poly @@ RationalPolynomial.overapprox (Map.find_exn t.inv_poly var)
            in
            Bound.substitute var ~replacement:bound_of_inv_poly bound)
          (Map.keys t.inv_poly) ~init:bound
end
