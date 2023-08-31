open OurBase
open Algebraic.Algebraic
open Polynomials

type 'a t = 'a mat
let of_list rows = mat
  (nat_of_integer @@ OurInt.of_int @@ List.length @@ List.hd_exn rows)
  (nat_of_integer @@ OurInt.of_int @@ List.length rows)
  (fun (col,row) ->
      Option.value_exn @@ List.nth (Option.value_exn @@ List.nth rows (OurInt.to_int @@ integer_of_nat row)) (OurInt.to_int @@ integer_of_nat col))

let mat_to_string_ca mat =
  let rows = mat_to_list mat in
  Util.sequence_to_string ~f:(fun col -> Util.sequence_to_string ~f:OurAlgebraicComplex.to_string (Sequence.of_list col)) (Sequence.of_list rows)

  let mat_to_string_int mat =
    let rows = mat_to_list mat in
    Util.sequence_to_string ~f:(fun col -> Util.sequence_to_string ~f:OurInt.to_string (Sequence.of_list col)) (Sequence.of_list rows)

let char_poly mat =
  let coeffs_int = coeffs_int @@ char_poly_int mat in
  Polynomial.of_coeff coeffs_int (Var.of_string "X")

let eigenvalues mat = Polynomial.roots @@ char_poly mat

let jordan_normal_form mat =
  let eigenvalues = eigenvalues mat in
  let (b,_) = schur_decomp mat eigenvalues in
  let tmp = triangular_to_jnf_vector_ca b in
  ()
