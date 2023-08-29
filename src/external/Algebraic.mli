module Uint32 :
  sig
    val less : int32 -> int32 -> bool
    val less_eq : int32 -> int32 -> bool
    val set_bit : int32 -> Z.t -> bool -> int32
    val shiftl : int32 -> Z.t -> int32
    val shiftr : int32 -> Z.t -> int32
    val shiftr_signed : int32 -> Z.t -> int32
    val test_bit : int32 -> Z.t -> bool
  end
module Uint64 :
  sig
    val less : int64 -> int64 -> bool
    val less_eq : int64 -> int64 -> bool
    val set_bit : int64 -> Z.t -> bool -> int64
    val shiftl : int64 -> Z.t -> int64
    val shiftr : int64 -> Z.t -> int64
    val shiftr_signed : int64 -> Z.t -> int64
    val test_bit : int64 -> Z.t -> bool
  end
module Integer_Bit :
  sig
    val test_bit : Z.t -> Z.t -> bool
    val shiftl : Z.t -> Z.t -> Z.t
    val shiftr : Z.t -> Z.t -> Z.t
  end
module Str_Literal :
  sig
    val implode : ('a -> char) -> 'a list -> string
    val explode : (char -> 'a) -> string -> 'a list
    val z_128 : Z.t
    val check_ascii : Z.t -> Z.t
    val char_of_ascii : Z.t -> char
    val ascii_of_char : char -> Z.t
    val literal_of_asciis : Z.t list -> string
    val asciis_of_literal : string -> Z.t list
  end
module Algebraic :
  sig
    type int
    type ordera = Eq | Lt | Gt
    type nat
    type char
    type rat
    type 'a poly
    type real_alg
    type real
    type complex
    type ('a, 'b) sum = Inl of 'a | Inr of 'b
    val croot_ca : Z.t -> complex -> complex
    val of_real_imag_ca : real_alg * real_alg -> complex
    val complex_roots_of_complex_poly : complex list -> complex list
    val complex_roots_of_real_poly : real_alg list -> complex list
    val abs_ra : real_alg -> real_alg
    val one_ca : complex
    val one_ra : real_alg
    val complex_roots_of_integer_poly : Z.t list -> complex list
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
    val of_integer_ca : Z.t -> complex
    val of_integer_ra : Z.t -> real_alg
    val is_rational_ra : real_alg -> bool
    val of_rational_ca : Z.t * Z.t -> complex
    val of_rational_ra : Z.t * Z.t -> real_alg
  end
