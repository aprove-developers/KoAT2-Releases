open OurBase
open Algebraic.Algebraic
open Polynomials

module MatrixOver (Value : PolyTypes.Ring) = struct
  type 'a t = 'a mat
  let of_list rows = mat
    (nat_of_integer @@ OurInt.of_int @@ List.length @@ List.hd_exn rows)
    (nat_of_integer @@ OurInt.of_int @@ List.length rows)
    (fun (col,row) ->
        Option.value_exn @@ List.nth (Option.value_exn @@ List.nth rows (OurInt.to_int @@ integer_of_nat row)) (OurInt.to_int @@ integer_of_nat col))

  let to_string mat =
    let rows = mat_to_list mat in
    Util.sequence_to_string ~f:(fun col -> Util.sequence_to_string ~f:Value.to_string (Sequence.of_list col)) (Sequence.of_list rows)
end


module IntMatrix = struct
  include MatrixOver (OurInt)

  module CAMatrix = MatrixOver (OurInt)

  let char_poly mat =
    let coeffs_int = coeffs_int @@ char_poly_int mat in
    Polynomial.of_coeff coeffs_int (Var.of_string "λ")

  let eigenvalues mat = Polynomial.roots @@ char_poly mat

  let jordan_normal_form mat =
    let eigenvalues = eigenvalues mat in
    Printf.printf "%s" (Util.sequence_to_string ~f:OurAlgebraicComplex.to_string (Sequence.of_list eigenvalues));
    let (b,_) = schur_decomp mat eigenvalues in
    Printf.printf "%s" (CAMatrix.to_string mat);
    let tmp = triangular_to_jnf_vector_ca b in
    ()
end
