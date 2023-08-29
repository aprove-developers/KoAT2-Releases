theory Algebraic
  imports
    Algebraic_Numbers.Algebraic_Numbers_External_Code
    Factor_Algebraic_Polynomial.Roots_of_Real_Complex_Poly    
    Cubic_Quartic_Equations.Complex_Roots
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

in OCaml module_name Algebraic 

end