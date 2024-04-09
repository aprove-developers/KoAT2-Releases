theory Algebraic
  imports
    Algebraic_Numbers.Algebraic_Numbers_External_Code
    Factor_Algebraic_Polynomial.Roots_of_Real_Complex_Poly
    Factor_Algebraic_Polynomial.Factor_Complex_Poly  
    Cubic_Quartic_Equations.Complex_Roots
    Jordan_Normal_Form.Gauss_Jordan_Elimination 
    Jordan_Normal_Form.Matrix_IArray_Impl
    Jordan_Normal_Form.Char_Poly
    Jordan_Normal_Form.Schur_Decomposition
    Jordan_Normal_Form.Jordan_Normal_Form_Existence
    Jordan_Normal_Form.Matrix_Kernel
    Jordan_Normal_Form.Jordan_Normal_Form_Uniqueness
begin

definition complex_roots_of_complex_poly :: "complex list \<Rightarrow> complex list" where
  "complex_roots_of_complex_poly = roots_of_complex_poly o poly_of_list" 

definition complex_roots_of_real_poly :: "real_alg list \<Rightarrow> complex list" where
  "complex_roots_of_real_poly = complex_roots_of_complex_poly o map (\<lambda> r. of_real_imag_ca (r,0))" 

definition complex_roots_of_rational_poly :: "(integer \<times> integer)list \<Rightarrow> complex list" where
  "complex_roots_of_rational_poly = complex_roots_of_rat_poly o poly_of_list o map 
     (\<lambda> (n,d). rat_of_int (int_of_integer n) / rat_of_int (int_of_integer d))" 

definition complex_roots_of_integer_poly :: "integer list \<Rightarrow> complex list" where
  "complex_roots_of_integer_poly = complex_roots_of_int_poly o poly_of_list o map int_of_integer" 

definition croot_ca :: "integer \<Rightarrow> complex \<Rightarrow> complex" where
  "croot_ca = croot o nat_of_integer" 

definition char_poly_int :: "integer mat \<Rightarrow> integer poly" where
  "char_poly_int = char_poly"

definition coeffs_int :: "integer poly \<Rightarrow> integer list" where
  "coeffs_int = coeffs"

definition schur_decomp :: "integer mat \<Rightarrow> complex list \<Rightarrow> complex mat \<times> complex mat \<times> complex mat" where 
  "schur_decomp A es = (case schur_decomposition (map_mat of_integer_ca A) es of (B,P,Q) \<Rightarrow> (B,P,Q))"

definition triangular_to_jnf_vector_ca :: "complex mat \<Rightarrow> (nat \<times> complex) list" where
  "triangular_to_jnf_vector_ca = triangular_to_jnf_vector"

definition dim_gen_eigenspace_ca :: "complex mat \<Rightarrow> complex \<Rightarrow> nat \<Rightarrow> nat" where
  "dim_gen_eigenspace_ca = dim_gen_eigenspace"

definition gen_eigenvector_ca :: "complex mat \<Rightarrow> complex \<Rightarrow> nat \<Rightarrow> complex mat" where
  "gen_eigenvector_ca A ev k = gauss_jordan_single ((char_matrix A ev) ^\<^sub>m k)"

definition mat_inv_ca :: "complex mat \<Rightarrow> complex mat option" where
  "mat_inv_ca = mat_inverse"

definition plus_mat :: "('a :: plus) mat \<Rightarrow> 'a mat \<Rightarrow> 'a mat" where
  "plus_mat A B \<equiv> mat (dim_row B) (dim_col B) (\<lambda> ij. A $$ ij + B $$ ij)"

definition gauss_jordan_single_ca :: "complex mat \<Rightarrow> complex mat" where
  "gauss_jordan_single_ca = gauss_jordan_single"

definition factor_poly :: "integer list \<Rightarrow> complex \<times> (complex \<times> nat) list" where
  "factor_poly xs = factor_complex_main (poly_of_list (map (complex_of_int o int_of_integer) xs))"

export_code 

  (* preliminary operations *)
  order.Eq order.Lt order.Gt \<comment> \<open>for comparison\<close>
  Inl Inr \<comment> \<open>make disjoint sums available for decomposition information\<close>

  (* real algebraic operations *)
  zero_ra
  one_ra
  of_integer_ra
  of_rational_ra
  plus_ra
  minus_ra
  uminus_ra
  times_ra
  divide_ra
  inverse_ra
  abs_ra
  floor_ra
  ceiling_ra
  minimum_ra
  maximum_ra
  equals_ra
  less_ra
  less_equal_ra
  compare_ra  
  root_ra
  show_ra
  is_rational_ra
  to_rational_ra
  sign_ra
  decompose_ra

  (* complex algebraic operations *)
  zero_ca
  one_ca
  imag_unit_ca
  of_integer_ca
  of_rational_ca
  of_real_imag_ca
  plus_ca
  minus_ca
  uminus_ca
  times_ca
  divide_ca
  inverse_ca
  equals_ca
  csqrt_ca
  croot_ca
  show_ca
  real_of_ca
  imag_of_ca

  (* roots of algebraic polynomials *)

  complex_roots_of_integer_poly  
  complex_roots_of_rational_poly
  complex_roots_of_real_poly
  complex_roots_of_complex_poly
  factor_poly
  coeffs_int

  (* JNF *)
  char_poly_int
  schur_decomp
  triangular_to_jnf_vector_ca
  dim_gen_eigenspace_ca

  (* Matrix *)
  mat
  map_mat
  mat_to_list
  nat_of_integer
  integer_of_nat
  dim_row
  dim_col
  index_mat
  upper_triangular
  row
  rows
  col
  cols
  zero_mat
  smult_mat
  transpose_mat
  mat_inv_ca
  plus_mat
  gauss_jordan_single_ca


  (* Vector *)
  vec
  map_vec
  vec_of_list
  list_of_vec
  dim_vec
  scalar_prod
  smult_vec
  vec_index

in OCaml module_name Algebraic

end