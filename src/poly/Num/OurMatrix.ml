open OurBase
open Koat2_external.Algebraic.Algebraic
open Polynomials

module MatrixOver (Value : PolyTypes.Ring) = struct
  module ValueVector = OurVector.VectorOver (Value)

  let of_list rows =
    mat
      (nat_of_integer @@ OurInt.of_int @@ List.length rows)
      (nat_of_integer @@ OurInt.of_int @@ List.length @@ List.hd_exn rows)
      (fun (row, col) ->
        Option.value_exn
        @@ List.nth
             (Option.value_exn @@ List.nth rows (OurInt.to_int @@ integer_of_nat row))
             (OurInt.to_int @@ integer_of_nat col))


  let of_vec_list rows = of_list @@ List.map rows ~f:ValueVector.to_list

  let of_fun m n f =
    mat
      (nat_of_integer @@ OurInt.of_int m)
      (nat_of_integer @@ OurInt.of_int n)
      (fun (x, y) -> f ((OurInt.to_int % integer_of_nat) x) ((OurInt.to_int % integer_of_nat) y))


  let transpose = transpose_mat
  let to_list = mat_to_list
  let map = map_mat
  let rows = rows
  let cols = cols
  let dim_row mat = (OurInt.to_int % integer_of_nat % dim_row) mat
  let dim_col mat = (OurInt.to_int % integer_of_nat % dim_col) mat

  exception MatrixIndexOutOfRange

  let row mat n =
    if n < 0 || n >= dim_row mat then
      raise MatrixIndexOutOfRange
    else
      row mat (nat_of_integer @@ OurInt.of_int n)


  let col mat m =
    if m < 0 || m >= dim_col mat then
      raise MatrixIndexOutOfRange
    else
      col mat (nat_of_integer @@ OurInt.of_int m)


  let index mat m n =
    if n < 0 || n >= dim_row mat || m < 0 || m >= dim_col mat then
      raise MatrixIndexOutOfRange
    else
      index_mat mat ((nat_of_integer % OurInt.of_int) m, (nat_of_integer % OurInt.of_int) n)


  let to_string mat =
    let rows = to_list mat in
    Util.sequence_to_string
      ~f:(fun col -> Util.sequence_to_string ~f:Value.to_string (Sequence.of_list col))
      (Sequence.of_list rows)


  let equal v w = List.equal (List.equal Value.equal) (to_list v) (to_list w)
  let smult scalar mat = smult_mat { times = Value.mul } scalar mat
  let add = plus_mat { plus = Value.add }
  let sub mat1 mat2 = add mat1 (smult Value.(neg one) mat2)

  let mul mat1 mat2 =
    of_fun (dim_row mat1) (dim_col mat2) (fun i j -> ValueVector.scalar_prod (row mat1 i) (col mat2 j))


  let mul_vec mat vec =
    ValueVector.of_fun (ValueVector.dim_vec vec) (fun i -> ValueVector.scalar_prod (row mat i) vec)


  (* TODO throw exception *)
  let of_vec vec = transpose @@ of_list [ ValueVector.to_list vec ]

  (* TODO determinante *)

  let is_upper_triangular mat = upper_triangular ({ zero = Value.zero }, { equal = Value.equal }) mat

  exception MatrixInvalidSize

  let zero_mat m n =
    if m < 1 || n < 1 then
      raise MatrixInvalidSize
    else
      zero_mat { zero = Value.zero } ((nat_of_integer % OurInt.of_int) m) ((nat_of_integer % OurInt.of_int) n)


  let identity n =
    if n < 1 then
      raise MatrixInvalidSize
    else
      of_fun n n (fun x y ->
          if x == y then
            Value.one
          else
            Value.zero)


  let pow mat =
    if dim_col mat != dim_row mat || dim_row mat < 1 || dim_col mat < 1 then
      raise MatrixInvalidSize
    else
      let rec f mat = function
        | 0 -> identity (dim_col mat)
        | 1 -> mat
        | n ->
            let b = f mat (n / 2) in
            mul (mul b b)
              (if n mod 2 = 0 then
                 identity (dim_col mat)
               else
                 mat)
      in
      f mat
end

module CAMatrix = struct
  include MatrixOver (OurAlgebraicComplex)
  module CAVector = OurVector.CAVector

  (* Computes a basis for the kernel of the matrix [a]. *)
  let kernel a =
    let row_echelon = gauss_jordan_single_ca a in
    let getLeadingIndexRow row =
      let row = CAVector.to_list @@ List.nth_exn (rows row_echelon) row in
      List.findi row ~f:(fun _ x -> OurAlgebraicComplex.(equal one x))
    in
    (* http://linear.pugetsound.edu/html/section-LI.html#theorem-BNS *)
    (* https://marksmath.org/classes/Fall2019LinearAlgebra/demos/null_space_basis.html *)
    let d =
      List.map (List.range 0 (dim_row a)) ~f:getLeadingIndexRow
      |> List.filter ~f:Option.is_some
      |> List.map ~f:(Tuple2.first % Option.value_exn)
    in
    let f = List.filter (List.range 0 (dim_col a)) ~f:(not % List.mem d ~equal:( == )) in
    let getEntry j i =
      if List.mem f i ~equal:( = ) then
        if i = List.nth_exn f j then
          OurAlgebraicComplex.one
        else
          OurAlgebraicComplex.zero
      else
        let k = Tuple2.first @@ List.findi_exn d ~f:(fun _ x -> x = i) in
        OurAlgebraicComplex.neg @@ index row_echelon k (List.nth_exn f j)
    in
    List.init (List.length f) ~f:(fun j -> CAVector.of_fun (dim_col a) (getEntry j))


  let inv = mat_inv_ca

  let eigen_mat mat eigen =
    if dim_col mat != dim_row mat then
      raise MatrixInvalidSize
    else
      sub mat (smult eigen (identity (dim_row mat)))
end

module IntMatrix = struct
  include MatrixOver (OurInt)

  let convertMatrixToCA mat =
    mat |> to_list |> List.map ~f:(List.map ~f:OurAlgebraicComplex.of_ourint) |> CAMatrix.of_list


  (* Computes det(I*λ - a) *)
  let char_poly a =
    let coeffs_int = coeffs_int @@ char_poly_int a in
    Polynomial.of_coeff coeffs_int (Var.of_string "λ")


  (* Computes a list xs of algebraic complex numbers, s.t., λ \in xs iff there exists an eigenvector ev != 0 with mat*ev = λ*ev. *)
  let eigenvalues mat = Polynomial.roots @@ char_poly mat (* TODO raise exception if dim row != dim col *)

  (* Computes JNF mat = p * j * p^-1 and returns (p,j,p^-1). *)
  let jordan_normal_form mat =
    let eigenvalues =
      List.map
        (Polynomial.roots_quantity (char_poly mat))
        ~f:(fun (eigen, quantity) -> List.init (OurInt.to_int quantity) ~f:(const eigen))
      |> List.concat
      |> List.sort ~compare:OurAlgebraicComplex.compare
    in
    (* TODO Remove Schur decomp and use kernel construction instead. *)
    (* Use Schur decomposition to obtain matrices w,p, and q s.t. [mat = p * w * q] *)
    let w, (p, q) =
      if is_upper_triangular mat then
        (convertMatrixToCA mat, (CAMatrix.identity (dim_col mat), CAMatrix.identity (dim_col mat)))
      else
        schur_decomp mat eigenvalues
    in
    (* There is a block of size n for the eigenvalue λ if we have the tuple (n,λ). *)
    let jordan_blocks =
      List.map ~f:(Tuple2.map1 (OurInt.to_int % integer_of_nat)) (triangular_to_jnf_vector_ca w)
      |> List.sort ~compare:(fun (m, x) (n, y) ->
             let compareXY = OurAlgebraicComplex.compare x y in
             if compareXY != 0 then
               compareXY
             else
               -compare m n)
    in
    (* Constructs j. *)
    let j =
      let rec compute_j n = function
        | [] -> []
        | (x, ev) :: xs ->
            List.init x ~f:(fun i ->
                List.init (dim_row mat) ~f:(fun j ->
                    if j == i + n then
                      ev
                    else if j == i + n + 1 && i < x - 1 then
                      OurAlgebraicComplex.one
                    else
                      OurAlgebraicComplex.zero))
            @ compute_j (n + x) xs
      in
      of_list @@ compute_j 0 jordan_blocks
    in
    (* Change of basis matrix *)
    let b =
      let compute_jordan_chain eigen_mat general_eigenvec n =
        List.init n ~f:(fun i -> CAMatrix.mul_vec (CAMatrix.pow eigen_mat i) general_eigenvec) |> List.rev
      in
      List.fold jordan_blocks
        ~init:(List.hd_exn eigenvalues, [], [])
        ~f:
          CAMatrix.(
            fun (previous_eigen, vecs, xs) (n, eigen) ->
              let vecs =
                if OurAlgebraicComplex.equal previous_eigen eigen then
                  vecs
                else
                  []
              in
              let eigen_mat = eigen_mat w eigen in
              if n == 1 then
                let res = List.find_exn (kernel eigen_mat) ~f:(not % List.mem vecs ~equal:CAVector.equal) in
                (eigen, res :: vecs, xs @ [ res ])
              else
                let eigen_mat_n1 = pow eigen_mat (n - 1) in
                let eigen_mat_n = mul eigen_mat_n1 eigen_mat in
                let general_eigenvec =
                  List.find_exn (kernel eigen_mat_n) ~f:(fun x ->
                      (not @@ CAVector.is_zero (mul_vec eigen_mat_n1 x))
                      && (not @@ List.mem vecs ~equal:CAVector.equal x))
                in
                let res = compute_jordan_chain eigen_mat general_eigenvec n in
                (eigen, res @ vecs, xs @ res))
      |> Tuple3.third |> CAMatrix.of_vec_list |> transpose_mat
    in
    CAMatrix.(mul p b, j, mul (Option.value_exn @@ inv b) q)
end

module CAPolyMatrix = struct
  module CAVector = OurVector.VectorOver (CAPolynomial)
  include MatrixOver (CAPolynomial)

  let of_CAMatrix = map CAPolynomial.of_constant

  let linear_map mat var_list =
    let res_vec = mul_vec (of_CAMatrix mat) (CAVector.of_list @@ List.map var_list ~f:CAPolynomial.of_var) in
    List.zip_exn var_list (CAVector.to_list res_vec)
end
