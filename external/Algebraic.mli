module Algebraic : sig
  type int
  type 'a times = { times : 'a -> 'a -> 'a }
  type ordera = Eq | Lt | Gt
  type 'a zero = { zero : 'a }
  type 'a equal = { equal : 'a -> 'a -> bool }
  type nat

  val integer_of_nat : nat -> Z.t
  val nat_of_integer : Z.t -> nat

  type char
  type 'a plus = { plus : 'a -> 'a -> 'a }
  type 'a semiring_0
  type rat
  type 'a poly
  type real_alg
  type real
  type 'a mat_impl
  type 'a mat
  type 'a vec_impl
  type 'a vec

  val vec_index : 'a vec -> nat -> 'a
  val mat : nat -> nat -> (nat * nat -> 'a) -> 'a mat
  val vec : nat -> (nat -> 'a) -> 'a vec
  val index_mat : 'a mat -> nat * nat -> 'a
  val dim_row : 'a mat -> nat
  val dim_col : 'a mat -> nat
  val transpose_mat : 'a mat -> 'a mat
  val list_of_vec : 'a vec -> 'a list

  type complex
  type ('a, 'b) sum = Inl of 'a | Inr of 'b

  val col : 'a mat -> nat -> 'a vec
  val row : 'a mat -> nat -> 'a vec
  val cols : 'a mat -> 'a vec list
  val rows : 'a mat -> 'a vec list
  val dim_vec : 'a vec -> nat
  val map_mat : ('a -> 'b) -> 'a mat -> 'b mat
  val map_vec : ('a -> 'b) -> 'a vec -> 'b vec
  val scalar_prod : 'a semiring_0 -> 'a vec -> 'a vec -> 'a
  val zero_mat : 'a zero -> nat -> nat -> 'a mat
  val smult_mat : 'a times -> 'a -> 'a mat -> 'a mat
  val smult_vec : 'a times -> 'a -> 'a vec -> 'a vec
  val croot_ca : Z.t -> complex -> complex
  val plus_mat : 'a plus -> 'a mat -> 'a mat -> 'a mat
  val mat_to_list : 'a mat -> 'a list list
  val vec_of_list : 'a list -> 'a vec
  val coeffs_int : Z.t poly -> Z.t list
  val mat_inv_ca : complex mat -> complex mat option
  val of_integer_ca : Z.t -> complex
  val schur_decomp : Z.t mat -> complex list -> complex mat * (complex mat * complex mat)
  val char_poly_int : Z.t mat -> Z.t poly
  val upper_triangular : 'a zero * 'a equal -> 'a mat -> bool
  val dim_gen_eigenspace_ca : complex mat -> complex -> nat -> nat
  val gauss_jordan_single_ca : complex mat -> complex mat
  val of_real_imag_ca : real_alg * real_alg -> complex
  val complex_roots_of_complex_poly : complex list -> complex list
  val complex_roots_of_real_poly : real_alg list -> complex list
  val triangular_to_jnf_vector_ca : complex mat -> (nat * complex) list
  val abs_ra : real_alg -> real_alg
  val one_ca : complex
  val one_ra : real_alg
  val complex_roots_of_integer_poly : Z.t list -> complex list
  val factor_poly : Z.t list -> complex * (complex * nat) list
  val less_ra : real_alg -> real_alg -> bool
  val plus_ca : complex -> complex -> complex
  val plus_ra : real_alg -> real_alg -> real_alg
  val root_ra : Z.t -> real_alg -> real_alg
  val show_ca : complex -> string
  val show_ra : real_alg -> string
  val to_rational_ra : real_alg -> Z.t * Z.t
  val sign_ra : real_alg -> Z.t
  val zero_ca : complex
  val zero_ra : real_alg
  val complex_roots_of_rational_poly : (Z.t * Z.t) list -> complex list
  val csqrt_ca : complex -> complex
  val floor_ra : real_alg -> Z.t
  val minus_ca : complex -> complex -> complex
  val minus_ra : real_alg -> real_alg -> real_alg
  val times_ca : complex -> complex -> complex
  val times_ra : real_alg -> real_alg -> real_alg
  val divide_ca : complex -> complex -> complex
  val divide_ra : real_alg -> real_alg -> real_alg
  val equals_ca : complex -> complex -> bool
  val equals_ra : real_alg -> real_alg -> bool
  val uminus_ca : complex -> complex
  val uminus_ra : real_alg -> real_alg
  val ceiling_ra : real_alg -> Z.t
  val compare_ra : real_alg -> real_alg -> ordera
  val imag_of_ca : complex -> real_alg
  val inverse_ca : complex -> complex
  val inverse_ra : real_alg -> real_alg
  val maximum_ra : real_alg -> real_alg -> real_alg
  val minimum_ra : real_alg -> real_alg -> real_alg
  val real_of_ca : complex -> real_alg
  val decompose_ra : real_alg -> (Z.t * Z.t, Z.t list * Z.t) sum
  val imag_unit_ca : complex
  val less_equal_ra : real_alg -> real_alg -> bool
  val of_integer_ra : Z.t -> real_alg
  val is_rational_ra : real_alg -> bool
  val of_rational_ca : Z.t * Z.t -> complex
  val of_rational_ra : Z.t * Z.t -> real_alg
end
