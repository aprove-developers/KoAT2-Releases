open OurBase
open Algebraic.Algebraic
open Polynomials

module MatrixOver (Value : PolyTypes.Ring) = struct
  let of_list rows = mat
    (nat_of_integer @@ OurInt.of_int @@ List.length @@ List.hd_exn rows)
    (nat_of_integer @@ OurInt.of_int @@ List.length rows)
    (fun (col,row) ->
        Option.value_exn @@ List.nth (Option.value_exn @@ List.nth rows (OurInt.to_int @@ integer_of_nat col)) (OurInt.to_int @@ integer_of_nat row))

  let to_list = mat_to_list
  let map = map_mat
  let rows = Algebraic.Algebraic.rows
  let cols = Algebraic.Algebraic.cols
  let dim_row mat = (OurInt.to_int % integer_of_nat % Algebraic.Algebraic.dim_row) mat
  let dim_col mat = (OurInt.to_int % integer_of_nat % Algebraic.Algebraic.dim_col) mat
  exception MatrixIndexOutOfRange
  let row mat n =
    if n < 0 || n >= dim_row mat then
      raise MatrixIndexOutOfRange
    else
      Algebraic.Algebraic.row mat (nat_of_integer @@ OurInt.of_int n)

  let col mat m =
    if m < 0 || m >= dim_col mat then
      raise MatrixIndexOutOfRange
    else
      Algebraic.Algebraic.col mat (nat_of_integer @@ OurInt.of_int m)

  let index mat m n =
    if n < 0 || n >= dim_row mat || m < 0 || m >= dim_col mat then
      raise MatrixIndexOutOfRange
    else
      index_mat mat ((nat_of_integer % OurInt.of_int)  m, (nat_of_integer % OurInt.of_int) n)

  let to_string mat =
    let rows = to_list mat in
    Util.sequence_to_string ~f:(fun col -> Util.sequence_to_string ~f:Value.to_string (Sequence.of_list col)) (Sequence.of_list rows)

  let smult scalar mat = smult_mat {times = Value.mul} scalar mat

  module OurVector = OurVec.VectorOver (Value)

  let add = plus_mat {plus = Value.add}
  let mul mat1 mat2 =
    mat ((nat_of_integer % OurInt.of_int) @@ dim_col mat1) ((nat_of_integer % OurInt.of_int) @@ dim_row mat2) (fun (i,j) -> OurVector.scalar_prod (row mat1 ((OurInt.to_int % integer_of_nat) i)) (col mat2 ((OurInt.to_int % integer_of_nat) j)))  (* TODO throw exception *)

  let transpose = transpose_mat

  (* TODO determinante *)

  let is_upper_triangular mat = upper_triangular ({zero = Value.zero}, {equal = Value.equal}) mat

  exception MatrixInvalidSize
  let zero_mat m n =
    if m < 1 || n < 1 then
      raise MatrixInvalidSize
    else
      zero_mat {zero = Value.zero} ((nat_of_integer % OurInt.of_int) m) ((nat_of_integer % OurInt.of_int) n)
end

module CAMatrix = struct
  include MatrixOver (OurAlgebraicComplex)

  (* Finds a basis for ax = b. *)
  let gauss_jordan =
    gauss_jordan_single_ca
      (* TODO *)

  let inv = mat_inv_ca
end

module IntMatrix = struct
  include MatrixOver (OurInt)

  module CAMatrix = MatrixOver (OurAlgebraicComplex)

  (* Computes det(I*λ - a) *)
  let char_poly a =
    let coeffs_int = coeffs_int @@ char_poly_int a in
    Polynomial.of_coeff coeffs_int (Var.of_string "λ")

  (* Computes a list xs of algebraic complex numbers, s.t., λ \in xs iff there exists an eigenvector ev != 0 with mat*ev = λ*ev. *)
  let eigenvalues mat = Polynomial.roots @@ char_poly mat

  (* Computes JNF mat = p * j * p^-1 and returns (p,j,p^-1). *)
  let jordan_normal_form mat =
    let eigenvalues = eigenvalues mat in
    Printf.printf "%s" (Util.sequence_to_string ~f:OurAlgebraicComplex.to_string (Sequence.of_list eigenvalues));
    let (b,_) = schur_decomp mat eigenvalues in
    Printf.printf "%s" (CAMatrix.to_string b);
    let jordan_blocks = triangular_to_jnf_vector_ca b in
    let dim_gen_eigenspace = List.map jordan_blocks ~f:(fun (algebraic_mul,eigen) -> dim_gen_eigenspace_ca b eigen algebraic_mul, eigen) in
    ()
end


