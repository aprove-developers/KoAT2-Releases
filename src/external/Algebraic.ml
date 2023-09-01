module Uint32 : sig
  val less : int32 -> int32 -> bool
  val less_eq : int32 -> int32 -> bool
  val set_bit : int32 -> Z.t -> bool -> int32
  val shiftl : int32 -> Z.t -> int32
  val shiftr : int32 -> Z.t -> int32
  val shiftr_signed : int32 -> Z.t -> int32
  val test_bit : int32 -> Z.t -> bool
end = struct

(* negative numbers have their highest bit set,
   so they are greater than positive ones *)
let less x y =
  if Int32.compare x Int32.zero < 0 then
    Int32.compare y Int32.zero < 0 && Int32.compare x y < 0
  else Int32.compare y Int32.zero < 0 || Int32.compare x y < 0;;

let less_eq x y =
  if Int32.compare x Int32.zero < 0 then
    Int32.compare y Int32.zero < 0 && Int32.compare x y <= 0
  else Int32.compare y Int32.zero < 0 || Int32.compare x y <= 0;;

let set_bit x n b =
  let mask = Int32.shift_left Int32.one (Z.to_int n)
  in if b then Int32.logor x mask
     else Int32.logand x (Int32.lognot mask);;

let shiftl x n = Int32.shift_left x (Z.to_int n);;

let shiftr x n = Int32.shift_right_logical x (Z.to_int n);;

let shiftr_signed x n = Int32.shift_right x (Z.to_int n);;

let test_bit x n =
  Int32.compare
    (Int32.logand x (Int32.shift_left Int32.one (Z.to_int n)))
    Int32.zero
  <> 0;;

end;; (*struct Uint32*)

module Uint64 : sig
  val less : int64 -> int64 -> bool
  val less_eq : int64 -> int64 -> bool
  val set_bit : int64 -> Z.t -> bool -> int64
  val shiftl : int64 -> Z.t -> int64
  val shiftr : int64 -> Z.t -> int64
  val shiftr_signed : int64 -> Z.t -> int64
  val test_bit : int64 -> Z.t -> bool
end = struct

(* negative numbers have their highest bit set,
   so they are greater than positive ones *)
let less x y =
  if Int64.compare x Int64.zero < 0 then
    Int64.compare y Int64.zero < 0 && Int64.compare x y < 0
  else Int64.compare y Int64.zero < 0 || Int64.compare x y < 0;;

let less_eq x y =
  if Int64.compare x Int64.zero < 0 then
    Int64.compare y Int64.zero < 0 && Int64.compare x y <= 0
  else Int64.compare y Int64.zero < 0 || Int64.compare x y <= 0;;

let set_bit x n b =
  let mask = Int64.shift_left Int64.one (Z.to_int n)
  in if b then Int64.logor x mask
     else Int64.logand x (Int64.lognot mask);;

let shiftl x n = Int64.shift_left x (Z.to_int n);;

let shiftr x n = Int64.shift_right_logical x (Z.to_int n);;

let shiftr_signed x n = Int64.shift_right x (Z.to_int n);;

let test_bit x n =
  Int64.compare
    (Int64.logand x (Int64.shift_left Int64.one (Z.to_int n)))
    Int64.zero
  <> 0;;

end;; (*struct Uint64*)

module Integer_Bit : sig
  val test_bit : Z.t -> Z.t -> bool
  val shiftl : Z.t -> Z.t -> Z.t
  val shiftr : Z.t -> Z.t -> Z.t
end = struct

(* We do not need an explicit range checks here,
   because Big_int.int_of_big_int raises Failure
   if the argument does not fit into an int. *)
let test_bit x n =  Z.testbit x (Z.to_int n);;

let shiftl x n = Z.shift_left x (Z.to_int n);;

let shiftr x n = Z.shift_right x (Z.to_int n);;

end;; (*struct Integer_Bit*)

module Str_Literal =
struct

let implode f xs =
  let rec length xs = match xs with
      [] -> 0
    | x :: xs -> 1 + length xs in
  let rec nth xs n = match xs with
    (x :: xs) -> if n <= 0 then x else nth xs (n - 1)
  in String.init (length xs) (fun n -> f (nth xs n));;

let explode f s =
  let rec map_range f n =
    if n <= 0 then [] else map_range f (n - 1) @ [f n]
  in map_range (fun n -> f (String.get s n)) (String.length s);;

let z_128 = Z.of_int 128;;

let check_ascii (k : Z.t) =
  if Z.leq Z.zero k && Z.lt k z_128
  then k
  else failwith "Non-ASCII character in literal";;

let char_of_ascii k = Char.chr (Z.to_int (check_ascii k));;

let ascii_of_char c = check_ascii (Z.of_int (Char.code c));;

let literal_of_asciis ks = implode char_of_ascii ks;;

let asciis_of_literal s = explode ascii_of_char s;;

end;;

module Algebraic : sig
  type int
  type 'a times = {times : 'a -> 'a -> 'a}
  type ordera = Eq | Lt | Gt
  type 'a zero = {zero : 'a}
  type 'a equal = {equal : 'a -> 'a -> bool}
  type nat
  val integer_of_nat : nat -> Z.t
  val nat_of_integer : Z.t -> nat
  type char
  type 'a plus = { plus : 'a -> 'a -> 'a; }
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
  val mat_to_list : 'a mat -> ('a list) list
  val vec_of_list : 'a list -> 'a vec
  val coeffs_int : Z.t poly -> Z.t list
  val mat_inv_ca : complex mat -> complex mat option
  val of_integer_ca : Z.t -> complex
  val schur_decomp :
    Z.t mat -> complex list -> complex mat * (complex mat * complex mat)
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
  val decompose_ra : real_alg -> ((Z.t * Z.t), (Z.t list * Z.t)) sum
  val imag_unit_ca : complex
  val less_equal_ra : real_alg -> real_alg -> bool
  val of_integer_ra : Z.t -> real_alg
  val is_rational_ra : real_alg -> bool
  val of_rational_ca : Z.t * Z.t -> complex
  val of_rational_ra : Z.t * Z.t -> real_alg
end = struct

type int = Int_of_integer of Z.t;;

let rec integer_of_int (Int_of_integer k) = k;;

let rec times_inta
  k l = Int_of_integer (Z.mul (integer_of_int k) (integer_of_int l));;

let zero_inta : int = Int_of_integer Z.zero;;

type num = One | Bit0 of num | Bit1 of num;;

let one_inta : int = Int_of_integer (Z.of_int 1);;

type 'a times = {times : 'a -> 'a -> 'a};;
let times _A = _A.times;;

let rec apsnd f (x, y) = (x, f y);;

let rec divmod_integer
  k l = (if Z.equal k Z.zero then (Z.zero, Z.zero)
          else (if Z.lt Z.zero l
                 then (if Z.lt Z.zero k then Z.div_rem k l
                        else (let (r, s) = Z.div_rem (Z.neg k) l in
                               (if Z.equal s Z.zero then (Z.neg r, Z.zero)
                                 else (Z.sub (Z.neg r) (Z.of_int 1),
Z.sub l s))))
                 else (if Z.equal l Z.zero then (Z.zero, k)
                        else apsnd Z.neg
                               (if Z.lt k Z.zero
                                 then Z.div_rem (Z.neg k) (Z.neg l)
                                 else (let (r, s) = Z.div_rem k (Z.neg l) in
(if Z.equal s Z.zero then (Z.neg r, Z.zero)
  else (Z.sub (Z.neg r) (Z.of_int 1), Z.sub (Z.neg l) s)))))));;

let rec fst (x1, x2) = x1;;

let rec divide_integera k l = fst (divmod_integer k l);;

let rec lcm_integer
  a b = divide_integera (Z.mul (Z.abs a) (Z.abs b))
          ((fun k l -> if Z.equal k Z.zero then Z.abs l else if Z.equal
             l Z.zero then Z.abs k else Z.gcd k l)
            a b);;

let rec lcm_inta
  (Int_of_integer x) (Int_of_integer y) = Int_of_integer (lcm_integer x y);;

let rec gcd_intc
  (Int_of_integer x) (Int_of_integer y) =
    Int_of_integer
      ((fun k l -> if Z.equal k Z.zero then Z.abs l else if Z.equal
         l Z.zero then Z.abs k else Z.gcd k l)
        x y);;

type color = R | B;;

type ('a, 'b) rbt = Empty |
  Branch of color * ('a, 'b) rbt * 'a * 'b * ('a, 'b) rbt;;

type ordera = Eq | Lt | Gt;;

type 'a ccompare = {ccompare : ('a -> 'a -> ordera) option};;
let ccompare _A = _A.ccompare;;

type ('b, 'a) mapping_rbt = Mapping_RBT of ('b, 'a) rbt;;

type 'a ceq = {ceq : ('a -> 'a -> bool) option};;
let ceq _A = _A.ceq;;

type 'a set_dlist = Abs_dlist of 'a list;;

type 'a set = Collect_set of ('a -> bool) | DList_set of 'a set_dlist |
  RBT_set of ('a, unit) mapping_rbt | Set_Monad of 'a list |
  Complement of 'a set;;

type 'a zero = {zero : 'a};;
let zero _A = _A.zero;;

type 'a one = {one : 'a};;
let one _A = _A.one;;

type 'a dvd = {times_dvd : 'a times};;

type 'a gcda =
  {one_gcd : 'a one; zero_gcd : 'a zero; dvd_gcd : 'a dvd;
    gcda : 'a -> 'a -> 'a; lcma : 'a -> 'a -> 'a};;
let gcda _A = _A.gcda;;
let lcma _A = _A.lcma;;

type 'a gcd = {gcd_Gcd : 'a gcda; gcd : 'a set -> 'a; lcm : 'a set -> 'a};;
let gcd _A = _A.gcd;;
let lcm _A = _A.lcm;;

let rec dummy_Lcm _A x = lcm _A x;;

let dummy_Gcd _ = failwith "Code_Abort_Gcd.dummy_Gcd";;

let rec gcd_intb x = dummy_Gcd x;;

let zero_int = ({zero = zero_inta} : int zero);;

let one_int = ({one = one_inta} : int one);;

let times_int = ({times = times_inta} : int times);;

let dvd_int = ({times_dvd = times_int} : int dvd);;

let gcd_inta =
  ({one_gcd = one_int; zero_gcd = zero_int; dvd_gcd = dvd_int; gcda = gcd_intc;
     lcma = lcm_inta}
    : int gcda);;

let rec gcd_int () =
  ({gcd_Gcd = gcd_inta; gcd = gcd_intb; lcm = lcm_int} : int gcd)
and lcm_int x = dummy_Lcm (gcd_int ()) x;;
let gcd_int = gcd_int ();;

let rec equal_inta k l = Z.equal (integer_of_int k) (integer_of_int l);;

type 'a equal = {equal : 'a -> 'a -> bool};;
let equal _A = _A.equal;;

let equal_int = ({equal = equal_inta} : int equal);;

let rec uminus_inta k = Int_of_integer (Z.neg (integer_of_int k));;

let rec snd (x1, x2) = x2;;

let rec modulo_integer k l = snd (divmod_integer k l);;

type nat = Nat of Z.t;;

let rec integer_of_nat (Nat x) = x;;

let rec modulo_nata
  m n = Nat (modulo_integer (integer_of_nat m) (integer_of_nat n));;

let rec divide_nata
  m n = Nat (divide_integera (integer_of_nat m) (integer_of_nat n));;

let rec equal_nata m n = Z.equal (integer_of_nat m) (integer_of_nat n);;

type 'a ord = {less_eq : 'a -> 'a -> bool; less : 'a -> 'a -> bool};;
let less_eq _A = _A.less_eq;;
let less _A = _A.less;;

let rec max _A a b = (if less_eq _A a b then b else a);;

let ord_integer = ({less_eq = Z.leq; less = Z.lt} : Z.t ord);;

let rec nat_of_integer k = Nat (max ord_integer Z.zero k);;

let zero_nata : nat = Nat Z.zero;;

let one_nata : nat = Nat (Z.of_int 1);;

type char = Chara of bool * bool * bool * bool * bool * bool * bool * bool;;

let rec string_of_digit
  n = (if equal_nata n zero_nata
        then [Chara (false, false, false, false, true, true, false, false)]
        else (if equal_nata n one_nata
               then [Chara (true, false, false, false, true, true, false,
                             false)]
               else (if equal_nata n (nat_of_integer (Z.of_int 2))
                      then [Chara (false, true, false, false, true, true, false,
                                    false)]
                      else (if equal_nata n (nat_of_integer (Z.of_int 3))
                             then [Chara (true, true, false, false, true, true,
   false, false)]
                             else (if equal_nata n (nat_of_integer (Z.of_int 4))
                                    then [Chara
    (false, false, true, false, true, true, false, false)]
                                    else (if equal_nata n
       (nat_of_integer (Z.of_int 5))
   then [Chara (true, false, true, false, true, true, false, false)]
   else (if equal_nata n (nat_of_integer (Z.of_int 6))
          then [Chara (false, true, true, false, true, true, false, false)]
          else (if equal_nata n (nat_of_integer (Z.of_int 7))
                 then [Chara (true, true, true, false, true, true, false,
                               false)]
                 else (if equal_nata n (nat_of_integer (Z.of_int 8))
                        then [Chara (false, false, false, true, true, true,
                                      false, false)]
                        else [Chara (true, false, false, true, true, true,
                                      false, false)])))))))));;

let rec less_nat m n = Z.lt (integer_of_nat m) (integer_of_nat n);;

let rec shows_string x = (fun a -> x @ a);;

let rec comp f g = (fun x -> f (g x));;

let rec showsp_nat
  p n = (if less_nat n (nat_of_integer (Z.of_int 10))
          then shows_string (string_of_digit n)
          else comp (showsp_nat p
                      (divide_nata n (nat_of_integer (Z.of_int 10))))
                 (shows_string
                   (string_of_digit
                     (modulo_nata n (nat_of_integer (Z.of_int 10))))));;

let rec less_int k l = Z.lt (integer_of_int k) (integer_of_int l);;

let rec nat k = Nat (max ord_integer Z.zero (integer_of_int k));;

let rec showsp_int
  p i = (if less_int i zero_inta
          then comp (shows_string
                      [Chara (true, false, true, true, false, true, false,
                               false)])
                 (showsp_nat p (nat (uminus_inta i)))
          else showsp_nat p (nat i));;

let rec shows_prec_int x = showsp_int x;;

let rec shows_sep
  s sep x2 = match s, sep, x2 with s, sep, [] -> shows_string []
    | s, sep, [x] -> s x
    | s, sep, x :: v :: va ->
        comp (comp (s x) sep) (shows_sep s sep (v :: va));;

let rec null = function [] -> true
               | x :: xs -> false;;

let rec shows_list_gen
  showsx e l s r xs =
    (if null xs then shows_string e
      else comp (comp (shows_string l) (shows_sep showsx (shows_string s) xs))
             (shows_string r));;

let rec showsp_list
  s p xs =
    shows_list_gen (s zero_nata)
      [Chara (true, true, false, true, true, false, true, false);
        Chara (true, false, true, true, true, false, true, false)]
      [Chara (true, true, false, true, true, false, true, false)]
      [Chara (false, false, true, true, false, true, false, false);
        Chara (false, false, false, false, false, true, false, false)]
      [Chara (true, false, true, true, true, false, true, false)] xs;;

let rec shows_list_int x = showsp_list shows_prec_int zero_nata x;;

type 'a show =
  {shows_prec : nat -> 'a -> char list -> char list;
    shows_list : 'a list -> char list -> char list};;
let shows_prec _A = _A.shows_prec;;
let shows_list _A = _A.shows_list;;

let show_int =
  ({shows_prec = shows_prec_int; shows_list = shows_list_int} : int show);;

let rec minus_inta
  k l = Int_of_integer (Z.sub (integer_of_int k) (integer_of_int l));;

let rec plus_inta
  k l = Int_of_integer (Z.add (integer_of_int k) (integer_of_int l));;

type 'a uminus = {uminus : 'a -> 'a};;
let uminus _A = _A.uminus;;

type 'a minus = {minus : 'a -> 'a -> 'a};;
let minus _A = _A.minus;;

type 'a plus = {plus : 'a -> 'a -> 'a};;
let plus _A = _A.plus;;

type 'a semigroup_add = {plus_semigroup_add : 'a plus};;

type 'a cancel_semigroup_add =
  {semigroup_add_cancel_semigroup_add : 'a semigroup_add};;

type 'a ab_semigroup_add = {semigroup_add_ab_semigroup_add : 'a semigroup_add};;

type 'a cancel_ab_semigroup_add =
  {ab_semigroup_add_cancel_ab_semigroup_add : 'a ab_semigroup_add;
    cancel_semigroup_add_cancel_ab_semigroup_add : 'a cancel_semigroup_add;
    minus_cancel_ab_semigroup_add : 'a minus};;

type 'a monoid_add =
  {semigroup_add_monoid_add : 'a semigroup_add; zero_monoid_add : 'a zero};;

type 'a comm_monoid_add =
  {ab_semigroup_add_comm_monoid_add : 'a ab_semigroup_add;
    monoid_add_comm_monoid_add : 'a monoid_add};;

type 'a cancel_comm_monoid_add =
  {cancel_ab_semigroup_add_cancel_comm_monoid_add : 'a cancel_ab_semigroup_add;
    comm_monoid_add_cancel_comm_monoid_add : 'a comm_monoid_add};;

type 'a mult_zero = {times_mult_zero : 'a times; zero_mult_zero : 'a zero};;

type 'a semigroup_mult = {times_semigroup_mult : 'a times};;

type 'a semiring =
  {ab_semigroup_add_semiring : 'a ab_semigroup_add;
    semigroup_mult_semiring : 'a semigroup_mult};;

type 'a semiring_0 =
  {comm_monoid_add_semiring_0 : 'a comm_monoid_add;
    mult_zero_semiring_0 : 'a mult_zero; semiring_semiring_0 : 'a semiring};;

type 'a semiring_0_cancel =
  {cancel_comm_monoid_add_semiring_0_cancel : 'a cancel_comm_monoid_add;
    semiring_0_semiring_0_cancel : 'a semiring_0};;

type 'a ab_semigroup_mult =
  {semigroup_mult_ab_semigroup_mult : 'a semigroup_mult};;

type 'a comm_semiring =
  {ab_semigroup_mult_comm_semiring : 'a ab_semigroup_mult;
    semiring_comm_semiring : 'a semiring};;

type 'a comm_semiring_0 =
  {comm_semiring_comm_semiring_0 : 'a comm_semiring;
    semiring_0_comm_semiring_0 : 'a semiring_0};;

type 'a comm_semiring_0_cancel =
  {comm_semiring_0_comm_semiring_0_cancel : 'a comm_semiring_0;
    semiring_0_cancel_comm_semiring_0_cancel : 'a semiring_0_cancel};;

type 'a power = {one_power : 'a one; times_power : 'a times};;

type 'a monoid_mult =
  {semigroup_mult_monoid_mult : 'a semigroup_mult;
    power_monoid_mult : 'a power};;

type 'a numeral =
  {one_numeral : 'a one; semigroup_add_numeral : 'a semigroup_add};;

type 'a semiring_numeral =
  {monoid_mult_semiring_numeral : 'a monoid_mult;
    numeral_semiring_numeral : 'a numeral;
    semiring_semiring_numeral : 'a semiring};;

type 'a zero_neq_one =
  {one_zero_neq_one : 'a one; zero_zero_neq_one : 'a zero};;

type 'a semiring_1 =
  {semiring_numeral_semiring_1 : 'a semiring_numeral;
    semiring_0_semiring_1 : 'a semiring_0;
    zero_neq_one_semiring_1 : 'a zero_neq_one};;

type 'a semiring_1_cancel =
  {semiring_0_cancel_semiring_1_cancel : 'a semiring_0_cancel;
    semiring_1_semiring_1_cancel : 'a semiring_1};;

type 'a comm_monoid_mult =
  {ab_semigroup_mult_comm_monoid_mult : 'a ab_semigroup_mult;
    monoid_mult_comm_monoid_mult : 'a monoid_mult;
    dvd_comm_monoid_mult : 'a dvd};;

type 'a comm_semiring_1 =
  {comm_monoid_mult_comm_semiring_1 : 'a comm_monoid_mult;
    comm_semiring_0_comm_semiring_1 : 'a comm_semiring_0;
    semiring_1_comm_semiring_1 : 'a semiring_1};;

type 'a comm_semiring_1_cancel =
  {comm_semiring_0_cancel_comm_semiring_1_cancel : 'a comm_semiring_0_cancel;
    comm_semiring_1_comm_semiring_1_cancel : 'a comm_semiring_1;
    semiring_1_cancel_comm_semiring_1_cancel : 'a semiring_1_cancel};;

type 'a comm_semiring_1_cancel_crossproduct =
  {comm_semiring_1_cancel_comm_semiring_1_cancel_crossproduct :
     'a comm_semiring_1_cancel};;

type 'a semiring_no_zero_divisors =
  {semiring_0_semiring_no_zero_divisors : 'a semiring_0};;

type 'a semiring_1_no_zero_divisors =
  {semiring_1_semiring_1_no_zero_divisors : 'a semiring_1;
    semiring_no_zero_divisors_semiring_1_no_zero_divisors :
      'a semiring_no_zero_divisors};;

type 'a semiring_no_zero_divisors_cancel =
  {semiring_no_zero_divisors_semiring_no_zero_divisors_cancel :
     'a semiring_no_zero_divisors};;

type 'a group_add =
  {cancel_semigroup_add_group_add : 'a cancel_semigroup_add;
    minus_group_add : 'a minus; monoid_add_group_add : 'a monoid_add;
    uminus_group_add : 'a uminus};;

type 'a ab_group_add =
  {cancel_comm_monoid_add_ab_group_add : 'a cancel_comm_monoid_add;
    group_add_ab_group_add : 'a group_add};;

type 'a ring =
  {ab_group_add_ring : 'a ab_group_add;
    semiring_0_cancel_ring : 'a semiring_0_cancel};;

type 'a ring_no_zero_divisors =
  {ring_ring_no_zero_divisors : 'a ring;
    semiring_no_zero_divisors_cancel_ring_no_zero_divisors :
      'a semiring_no_zero_divisors_cancel};;

type 'a neg_numeral =
  {group_add_neg_numeral : 'a group_add; numeral_neg_numeral : 'a numeral};;

type 'a ring_1 =
  {neg_numeral_ring_1 : 'a neg_numeral; ring_ring_1 : 'a ring;
    semiring_1_cancel_ring_1 : 'a semiring_1_cancel};;

type 'a ring_1_no_zero_divisors =
  {ring_1_ring_1_no_zero_divisors : 'a ring_1;
    ring_no_zero_divisors_ring_1_no_zero_divisors : 'a ring_no_zero_divisors;
    semiring_1_no_zero_divisors_ring_1_no_zero_divisors :
      'a semiring_1_no_zero_divisors};;

type 'a comm_ring =
  {comm_semiring_0_cancel_comm_ring : 'a comm_semiring_0_cancel;
    ring_comm_ring : 'a ring};;

type 'a comm_ring_1 =
  {comm_ring_comm_ring_1 : 'a comm_ring;
    comm_semiring_1_cancel_comm_ring_1 : 'a comm_semiring_1_cancel;
    ring_1_comm_ring_1 : 'a ring_1};;

type 'a semidom =
  {comm_semiring_1_cancel_semidom : 'a comm_semiring_1_cancel;
    semiring_1_no_zero_divisors_semidom : 'a semiring_1_no_zero_divisors};;

type 'a idom =
  {comm_ring_1_idom : 'a comm_ring_1;
    ring_1_no_zero_divisors_idom : 'a ring_1_no_zero_divisors;
    semidom_idom : 'a semidom;
    comm_semiring_1_cancel_crossproduct_idom :
      'a comm_semiring_1_cancel_crossproduct};;

let plus_int = ({plus = plus_inta} : int plus);;

let semigroup_add_int = ({plus_semigroup_add = plus_int} : int semigroup_add);;

let cancel_semigroup_add_int =
  ({semigroup_add_cancel_semigroup_add = semigroup_add_int} :
    int cancel_semigroup_add);;

let ab_semigroup_add_int =
  ({semigroup_add_ab_semigroup_add = semigroup_add_int} :
    int ab_semigroup_add);;

let minus_int = ({minus = minus_inta} : int minus);;

let cancel_ab_semigroup_add_int =
  ({ab_semigroup_add_cancel_ab_semigroup_add = ab_semigroup_add_int;
     cancel_semigroup_add_cancel_ab_semigroup_add = cancel_semigroup_add_int;
     minus_cancel_ab_semigroup_add = minus_int}
    : int cancel_ab_semigroup_add);;

let monoid_add_int =
  ({semigroup_add_monoid_add = semigroup_add_int; zero_monoid_add = zero_int} :
    int monoid_add);;

let comm_monoid_add_int =
  ({ab_semigroup_add_comm_monoid_add = ab_semigroup_add_int;
     monoid_add_comm_monoid_add = monoid_add_int}
    : int comm_monoid_add);;

let cancel_comm_monoid_add_int =
  ({cancel_ab_semigroup_add_cancel_comm_monoid_add =
      cancel_ab_semigroup_add_int;
     comm_monoid_add_cancel_comm_monoid_add = comm_monoid_add_int}
    : int cancel_comm_monoid_add);;

let mult_zero_int =
  ({times_mult_zero = times_int; zero_mult_zero = zero_int} : int mult_zero);;

let semigroup_mult_int =
  ({times_semigroup_mult = times_int} : int semigroup_mult);;

let semiring_int =
  ({ab_semigroup_add_semiring = ab_semigroup_add_int;
     semigroup_mult_semiring = semigroup_mult_int}
    : int semiring);;

let semiring_0_int =
  ({comm_monoid_add_semiring_0 = comm_monoid_add_int;
     mult_zero_semiring_0 = mult_zero_int; semiring_semiring_0 = semiring_int}
    : int semiring_0);;

let semiring_0_cancel_int =
  ({cancel_comm_monoid_add_semiring_0_cancel = cancel_comm_monoid_add_int;
     semiring_0_semiring_0_cancel = semiring_0_int}
    : int semiring_0_cancel);;

let ab_semigroup_mult_int =
  ({semigroup_mult_ab_semigroup_mult = semigroup_mult_int} :
    int ab_semigroup_mult);;

let comm_semiring_int =
  ({ab_semigroup_mult_comm_semiring = ab_semigroup_mult_int;
     semiring_comm_semiring = semiring_int}
    : int comm_semiring);;

let comm_semiring_0_int =
  ({comm_semiring_comm_semiring_0 = comm_semiring_int;
     semiring_0_comm_semiring_0 = semiring_0_int}
    : int comm_semiring_0);;

let comm_semiring_0_cancel_int =
  ({comm_semiring_0_comm_semiring_0_cancel = comm_semiring_0_int;
     semiring_0_cancel_comm_semiring_0_cancel = semiring_0_cancel_int}
    : int comm_semiring_0_cancel);;

let power_int = ({one_power = one_int; times_power = times_int} : int power);;

let monoid_mult_int =
  ({semigroup_mult_monoid_mult = semigroup_mult_int;
     power_monoid_mult = power_int}
    : int monoid_mult);;

let numeral_int =
  ({one_numeral = one_int; semigroup_add_numeral = semigroup_add_int} :
    int numeral);;

let semiring_numeral_int =
  ({monoid_mult_semiring_numeral = monoid_mult_int;
     numeral_semiring_numeral = numeral_int;
     semiring_semiring_numeral = semiring_int}
    : int semiring_numeral);;

let zero_neq_one_int =
  ({one_zero_neq_one = one_int; zero_zero_neq_one = zero_int} :
    int zero_neq_one);;

let semiring_1_int =
  ({semiring_numeral_semiring_1 = semiring_numeral_int;
     semiring_0_semiring_1 = semiring_0_int;
     zero_neq_one_semiring_1 = zero_neq_one_int}
    : int semiring_1);;

let semiring_1_cancel_int =
  ({semiring_0_cancel_semiring_1_cancel = semiring_0_cancel_int;
     semiring_1_semiring_1_cancel = semiring_1_int}
    : int semiring_1_cancel);;

let comm_monoid_mult_int =
  ({ab_semigroup_mult_comm_monoid_mult = ab_semigroup_mult_int;
     monoid_mult_comm_monoid_mult = monoid_mult_int;
     dvd_comm_monoid_mult = dvd_int}
    : int comm_monoid_mult);;

let comm_semiring_1_int =
  ({comm_monoid_mult_comm_semiring_1 = comm_monoid_mult_int;
     comm_semiring_0_comm_semiring_1 = comm_semiring_0_int;
     semiring_1_comm_semiring_1 = semiring_1_int}
    : int comm_semiring_1);;

let comm_semiring_1_cancel_int =
  ({comm_semiring_0_cancel_comm_semiring_1_cancel = comm_semiring_0_cancel_int;
     comm_semiring_1_comm_semiring_1_cancel = comm_semiring_1_int;
     semiring_1_cancel_comm_semiring_1_cancel = semiring_1_cancel_int}
    : int comm_semiring_1_cancel);;

let comm_semiring_1_cancel_crossproduct_int =
  ({comm_semiring_1_cancel_comm_semiring_1_cancel_crossproduct =
      comm_semiring_1_cancel_int}
    : int comm_semiring_1_cancel_crossproduct);;

let semiring_no_zero_divisors_int =
  ({semiring_0_semiring_no_zero_divisors = semiring_0_int} :
    int semiring_no_zero_divisors);;

let semiring_1_no_zero_divisors_int =
  ({semiring_1_semiring_1_no_zero_divisors = semiring_1_int;
     semiring_no_zero_divisors_semiring_1_no_zero_divisors =
       semiring_no_zero_divisors_int}
    : int semiring_1_no_zero_divisors);;

let semiring_no_zero_divisors_cancel_int =
  ({semiring_no_zero_divisors_semiring_no_zero_divisors_cancel =
      semiring_no_zero_divisors_int}
    : int semiring_no_zero_divisors_cancel);;

let uminus_int = ({uminus = uminus_inta} : int uminus);;

let group_add_int =
  ({cancel_semigroup_add_group_add = cancel_semigroup_add_int;
     minus_group_add = minus_int; monoid_add_group_add = monoid_add_int;
     uminus_group_add = uminus_int}
    : int group_add);;

let ab_group_add_int =
  ({cancel_comm_monoid_add_ab_group_add = cancel_comm_monoid_add_int;
     group_add_ab_group_add = group_add_int}
    : int ab_group_add);;

let ring_int =
  ({ab_group_add_ring = ab_group_add_int;
     semiring_0_cancel_ring = semiring_0_cancel_int}
    : int ring);;

let ring_no_zero_divisors_int =
  ({ring_ring_no_zero_divisors = ring_int;
     semiring_no_zero_divisors_cancel_ring_no_zero_divisors =
       semiring_no_zero_divisors_cancel_int}
    : int ring_no_zero_divisors);;

let neg_numeral_int =
  ({group_add_neg_numeral = group_add_int; numeral_neg_numeral = numeral_int} :
    int neg_numeral);;

let ring_1_int =
  ({neg_numeral_ring_1 = neg_numeral_int; ring_ring_1 = ring_int;
     semiring_1_cancel_ring_1 = semiring_1_cancel_int}
    : int ring_1);;

let ring_1_no_zero_divisors_int =
  ({ring_1_ring_1_no_zero_divisors = ring_1_int;
     ring_no_zero_divisors_ring_1_no_zero_divisors = ring_no_zero_divisors_int;
     semiring_1_no_zero_divisors_ring_1_no_zero_divisors =
       semiring_1_no_zero_divisors_int}
    : int ring_1_no_zero_divisors);;

let comm_ring_int =
  ({comm_semiring_0_cancel_comm_ring = comm_semiring_0_cancel_int;
     ring_comm_ring = ring_int}
    : int comm_ring);;

let comm_ring_1_int =
  ({comm_ring_comm_ring_1 = comm_ring_int;
     comm_semiring_1_cancel_comm_ring_1 = comm_semiring_1_cancel_int;
     ring_1_comm_ring_1 = ring_1_int}
    : int comm_ring_1);;

let semidom_int =
  ({comm_semiring_1_cancel_semidom = comm_semiring_1_cancel_int;
     semiring_1_no_zero_divisors_semidom = semiring_1_no_zero_divisors_int}
    : int semidom);;

let idom_int =
  ({comm_ring_1_idom = comm_ring_1_int;
     ring_1_no_zero_divisors_idom = ring_1_no_zero_divisors_int;
     semidom_idom = semidom_int;
     comm_semiring_1_cancel_crossproduct_idom =
       comm_semiring_1_cancel_crossproduct_int}
    : int idom);;

let rec abs_int i = (if less_int i zero_inta then uminus_inta i else i);;

let rec normalize_int x = abs_int x;;

let rec sgn_int
  i = (if equal_inta i zero_inta then zero_inta
        else (if less_int zero_inta i then one_inta
               else uminus_inta one_inta));;

let rec unit_factor_inta x = sgn_int x;;

let rec divide_inta
  k l = Int_of_integer (divide_integera (integer_of_int k) (integer_of_int l));;

type 'a divide = {divide : 'a -> 'a -> 'a};;
let divide _A = _A.divide;;

type 'a semidom_divide =
  {divide_semidom_divide : 'a divide; semidom_semidom_divide : 'a semidom;
    semiring_no_zero_divisors_cancel_semidom_divide :
      'a semiring_no_zero_divisors_cancel};;

type 'a unit_factor = {unit_factor : 'a -> 'a};;
let unit_factor _A = _A.unit_factor;;

type 'a semidom_divide_unit_factor =
  {semidom_divide_semidom_divide_unit_factor : 'a semidom_divide;
    unit_factor_semidom_divide_unit_factor : 'a unit_factor};;

type 'a algebraic_semidom =
  {semidom_divide_algebraic_semidom : 'a semidom_divide};;

type 'a normalization_semidom =
  {algebraic_semidom_normalization_semidom : 'a algebraic_semidom;
    semidom_divide_unit_factor_normalization_semidom :
      'a semidom_divide_unit_factor;
    normalizea : 'a -> 'a};;
let normalizea _A = _A.normalizea;;

type 'a comm_monoid_gcd =
  {gcd_comm_monoid_gcd : 'a gcda;
    comm_semiring_1_comm_monoid_gcd : 'a comm_semiring_1};;

type 'a idom_gcd =
  {idom_idom_gcd : 'a idom; comm_monoid_gcd_idom_gcd : 'a comm_monoid_gcd};;

type 'a semiring_gcd =
  {normalization_semidom_semiring_gcd : 'a normalization_semidom;
    comm_monoid_gcd_semiring_gcd : 'a comm_monoid_gcd};;

type 'a ring_gcd =
  {semiring_gcd_ring_gcd : 'a semiring_gcd; idom_gcd_ring_gcd : 'a idom_gcd};;

let comm_monoid_gcd_int =
  ({gcd_comm_monoid_gcd = gcd_inta;
     comm_semiring_1_comm_monoid_gcd = comm_semiring_1_int}
    : int comm_monoid_gcd);;

let idom_gcd_int =
  ({idom_idom_gcd = idom_int; comm_monoid_gcd_idom_gcd = comm_monoid_gcd_int} :
    int idom_gcd);;

let divide_int = ({divide = divide_inta} : int divide);;

let semidom_divide_int =
  ({divide_semidom_divide = divide_int; semidom_semidom_divide = semidom_int;
     semiring_no_zero_divisors_cancel_semidom_divide =
       semiring_no_zero_divisors_cancel_int}
    : int semidom_divide);;

let unit_factor_int = ({unit_factor = unit_factor_inta} : int unit_factor);;

let semidom_divide_unit_factor_int =
  ({semidom_divide_semidom_divide_unit_factor = semidom_divide_int;
     unit_factor_semidom_divide_unit_factor = unit_factor_int}
    : int semidom_divide_unit_factor);;

let algebraic_semidom_int =
  ({semidom_divide_algebraic_semidom = semidom_divide_int} :
    int algebraic_semidom);;

let normalization_semidom_int =
  ({algebraic_semidom_normalization_semidom = algebraic_semidom_int;
     semidom_divide_unit_factor_normalization_semidom =
       semidom_divide_unit_factor_int;
     normalizea = normalize_int}
    : int normalization_semidom);;

let semiring_gcd_int =
  ({normalization_semidom_semiring_gcd = normalization_semidom_int;
     comm_monoid_gcd_semiring_gcd = comm_monoid_gcd_int}
    : int semiring_gcd);;

let ring_gcd_int =
  ({semiring_gcd_ring_gcd = semiring_gcd_int; idom_gcd_ring_gcd = idom_gcd_int}
    : int ring_gcd);;

let rec modulo_inta
  k l = Int_of_integer (modulo_integer (integer_of_int k) (integer_of_int l));;

type 'a modulo =
  {divide_modulo : 'a divide; dvd_modulo : 'a dvd; modulo : 'a -> 'a -> 'a};;
let modulo _A = _A.modulo;;

let modulo_int =
  ({divide_modulo = divide_int; dvd_modulo = dvd_int; modulo = modulo_inta} :
    int modulo);;

let rec less_eq_int k l = Z.leq (integer_of_int k) (integer_of_int l);;

let ord_int = ({less_eq = less_eq_int; less = less_int} : int ord);;

type 'a preorder = {ord_preorder : 'a ord};;

type 'a order = {preorder_order : 'a preorder};;

let preorder_int = ({ord_preorder = ord_int} : int preorder);;

let order_int = ({preorder_order = preorder_int} : int order);;

type 'a semiring_Gcd =
  {gcd_semiring_Gcd : 'a gcd; semiring_gcd_semiring_Gcd : 'a semiring_gcd};;

let semiring_Gcd_int =
  ({gcd_semiring_Gcd = gcd_int; semiring_gcd_semiring_Gcd = semiring_gcd_int} :
    int semiring_Gcd);;

let ceq_inta : (int -> int -> bool) option = Some equal_inta;;

let ceq_int = ({ceq = ceq_inta} : int ceq);;

type 'a idom_divide =
  {idom_idom_divide : 'a idom; semidom_divide_idom_divide : 'a semidom_divide};;

let idom_divide_int =
  ({idom_idom_divide = idom_int;
     semidom_divide_idom_divide = semidom_divide_int}
    : int idom_divide);;

type 'a semiring_modulo =
  {comm_semiring_1_cancel_semiring_modulo : 'a comm_semiring_1_cancel;
    modulo_semiring_modulo : 'a modulo};;

type 'a semidom_modulo =
  {algebraic_semidom_semidom_modulo : 'a algebraic_semidom;
    semiring_modulo_semidom_modulo : 'a semiring_modulo};;

type 'a idom_modulo =
  {idom_divide_idom_modulo : 'a idom_divide;
    semidom_modulo_idom_modulo : 'a semidom_modulo};;

let semiring_modulo_int =
  ({comm_semiring_1_cancel_semiring_modulo = comm_semiring_1_cancel_int;
     modulo_semiring_modulo = modulo_int}
    : int semiring_modulo);;

let semidom_modulo_int =
  ({algebraic_semidom_semidom_modulo = algebraic_semidom_int;
     semiring_modulo_semidom_modulo = semiring_modulo_int}
    : int semidom_modulo);;

let idom_modulo_int =
  ({idom_divide_idom_modulo = idom_divide_int;
     semidom_modulo_idom_modulo = semidom_modulo_int}
    : int idom_modulo);;

type ('a, 'b) phantom = Phantom of 'b;;

type set_impla = Set_Choose | Set_Collect | Set_DList | Set_RBT | Set_Monada;;

let set_impl_inta : (int, set_impla) phantom = Phantom Set_RBT;;

type 'a set_impl = {set_impl : ('a, set_impla) phantom};;
let set_impl _A = _A.set_impl;;

let set_impl_int = ({set_impl = set_impl_inta} : int set_impl);;

type 'a linorder = {order_linorder : 'a order};;

let linorder_int = ({order_linorder = order_int} : int linorder);;

let rec eq _A a b = equal _A a b;;

let rec comparator_of (_A1, _A2)
  x y = (if less _A2.order_linorder.preorder_order.ord_preorder x y then Lt
          else (if eq _A1 x y then Eq else Gt));;

let rec compare_int x = comparator_of (equal_int, linorder_int) x;;

let ccompare_inta : (int -> int -> ordera) option = Some compare_int;;

let ccompare_int = ({ccompare = ccompare_inta} : int ccompare);;

type 'a normalization_semidom_multiplicative =
  {normalization_semidom_normalization_semidom_multiplicative :
     'a normalization_semidom};;

type 'a semiring_gcd_mult_normalize =
  {semiring_gcd_semiring_gcd_mult_normalize : 'a semiring_gcd;
    normalization_semidom_multiplicative_semiring_gcd_mult_normalize :
      'a normalization_semidom_multiplicative};;

let normalization_semidom_multiplicative_int =
  ({normalization_semidom_normalization_semidom_multiplicative =
      normalization_semidom_int}
    : int normalization_semidom_multiplicative);;

let semiring_gcd_mult_normalize_int =
  ({semiring_gcd_semiring_gcd_mult_normalize = semiring_gcd_int;
     normalization_semidom_multiplicative_semiring_gcd_mult_normalize =
       normalization_semidom_multiplicative_int}
    : int semiring_gcd_mult_normalize);;

let rec euclidean_size_int x = comp nat abs_int x;;

type 'a euclidean_semiring =
  {semidom_modulo_euclidean_semiring : 'a semidom_modulo;
    euclidean_size : 'a -> nat};;
let euclidean_size _A = _A.euclidean_size;;

type 'a euclidean_ring =
  {euclidean_semiring_euclidean_ring : 'a euclidean_semiring;
    idom_modulo_euclidean_ring : 'a idom_modulo};;

let euclidean_semiring_int =
  ({semidom_modulo_euclidean_semiring = semidom_modulo_int;
     euclidean_size = euclidean_size_int}
    : int euclidean_semiring);;

let euclidean_ring_int =
  ({euclidean_semiring_euclidean_ring = euclidean_semiring_int;
     idom_modulo_euclidean_ring = idom_modulo_int}
    : int euclidean_ring);;

type 'a factorial_semiring =
  {normalization_semidom_factorial_semiring : 'a normalization_semidom};;

type 'a factorial_semiring_gcd =
  {factorial_semiring_factorial_semiring_gcd : 'a factorial_semiring;
    semiring_Gcd_factorial_semiring_gcd : 'a semiring_Gcd};;

type 'a factorial_ring_gcd =
  {factorial_semiring_gcd_factorial_ring_gcd : 'a factorial_semiring_gcd;
    ring_gcd_factorial_ring_gcd : 'a ring_gcd;
    idom_divide_factorial_ring_gcd : 'a idom_divide};;

let factorial_semiring_int =
  ({normalization_semidom_factorial_semiring = normalization_semidom_int} :
    int factorial_semiring);;

let factorial_semiring_gcd_int =
  ({factorial_semiring_factorial_semiring_gcd = factorial_semiring_int;
     semiring_Gcd_factorial_semiring_gcd = semiring_Gcd_int}
    : int factorial_semiring_gcd);;

let factorial_ring_gcd_int =
  ({factorial_semiring_gcd_factorial_ring_gcd = factorial_semiring_gcd_int;
     ring_gcd_factorial_ring_gcd = ring_gcd_int;
     idom_divide_factorial_ring_gcd = idom_divide_int}
    : int factorial_ring_gcd);;

type 'a normalization_euclidean_semiring =
  {euclidean_semiring_normalization_euclidean_semiring : 'a euclidean_semiring;
    factorial_semiring_normalization_euclidean_semiring :
      'a factorial_semiring};;

type 'a euclidean_semiring_gcd =
  {normalization_euclidean_semiring_euclidean_semiring_gcd :
     'a normalization_euclidean_semiring;
    factorial_semiring_gcd_euclidean_semiring_gcd : 'a factorial_semiring_gcd};;

type 'a euclidean_ring_gcd =
  {euclidean_semiring_gcd_euclidean_ring_gcd : 'a euclidean_semiring_gcd;
    euclidean_ring_euclidean_ring_gcd : 'a euclidean_ring;
    factorial_ring_gcd_euclidean_ring_gcd : 'a factorial_ring_gcd};;

let normalization_euclidean_semiring_int =
  ({euclidean_semiring_normalization_euclidean_semiring =
      euclidean_semiring_int;
     factorial_semiring_normalization_euclidean_semiring =
       factorial_semiring_int}
    : int normalization_euclidean_semiring);;

let euclidean_semiring_gcd_int =
  ({normalization_euclidean_semiring_euclidean_semiring_gcd =
      normalization_euclidean_semiring_int;
     factorial_semiring_gcd_euclidean_semiring_gcd = factorial_semiring_gcd_int}
    : int euclidean_semiring_gcd);;

let euclidean_ring_gcd_int =
  ({euclidean_semiring_gcd_euclidean_ring_gcd = euclidean_semiring_gcd_int;
     euclidean_ring_euclidean_ring_gcd = euclidean_ring_int;
     factorial_ring_gcd_euclidean_ring_gcd = factorial_ring_gcd_int}
    : int euclidean_ring_gcd);;

let equal_nat = ({equal = equal_nata} : nat equal);;

let rec times_nata m n = Nat (Z.mul (integer_of_nat m) (integer_of_nat n));;

let times_nat = ({times = times_nata} : nat times);;

let dvd_nat = ({times_dvd = times_nat} : nat dvd);;

let one_nat = ({one = one_nata} : nat one);;

let rec plus_nata m n = Nat (Z.add (integer_of_nat m) (integer_of_nat n));;

let plus_nat = ({plus = plus_nata} : nat plus);;

let zero_nat = ({zero = zero_nata} : nat zero);;

let semigroup_add_nat = ({plus_semigroup_add = plus_nat} : nat semigroup_add);;

let numeral_nat =
  ({one_numeral = one_nat; semigroup_add_numeral = semigroup_add_nat} :
    nat numeral);;

let power_nat = ({one_power = one_nat; times_power = times_nat} : nat power);;

let rec minus_nata
  m n = Nat (max ord_integer Z.zero
              (Z.sub (integer_of_nat m) (integer_of_nat n)));;

let minus_nat = ({minus = minus_nata} : nat minus);;

let rec min _A a b = (if less_eq _A a b then a else b);;

let rec less_eq_nat m n = Z.leq (integer_of_nat m) (integer_of_nat n);;

let ord_nat = ({less_eq = less_eq_nat; less = less_nat} : nat ord);;

let rec inf_nata x = min ord_nat x;;

type 'a inf = {inf : 'a -> 'a -> 'a};;
let inf _A = _A.inf;;

let inf_nat = ({inf = inf_nata} : nat inf);;

let rec sup_nata x = max ord_nat x;;

type 'a sup = {sup : 'a -> 'a -> 'a};;
let sup _A = _A.sup;;

let sup_nat = ({sup = sup_nata} : nat sup);;

let divide_nat = ({divide = divide_nata} : nat divide);;

let modulo_nat =
  ({divide_modulo = divide_nat; dvd_modulo = dvd_nat; modulo = modulo_nata} :
    nat modulo);;

let ab_semigroup_add_nat =
  ({semigroup_add_ab_semigroup_add = semigroup_add_nat} :
    nat ab_semigroup_add);;

let monoid_add_nat =
  ({semigroup_add_monoid_add = semigroup_add_nat; zero_monoid_add = zero_nat} :
    nat monoid_add);;

let comm_monoid_add_nat =
  ({ab_semigroup_add_comm_monoid_add = ab_semigroup_add_nat;
     monoid_add_comm_monoid_add = monoid_add_nat}
    : nat comm_monoid_add);;

let mult_zero_nat =
  ({times_mult_zero = times_nat; zero_mult_zero = zero_nat} : nat mult_zero);;

let semigroup_mult_nat =
  ({times_semigroup_mult = times_nat} : nat semigroup_mult);;

let semiring_nat =
  ({ab_semigroup_add_semiring = ab_semigroup_add_nat;
     semigroup_mult_semiring = semigroup_mult_nat}
    : nat semiring);;

let semiring_0_nat =
  ({comm_monoid_add_semiring_0 = comm_monoid_add_nat;
     mult_zero_semiring_0 = mult_zero_nat; semiring_semiring_0 = semiring_nat}
    : nat semiring_0);;

let semiring_no_zero_divisors_nat =
  ({semiring_0_semiring_no_zero_divisors = semiring_0_nat} :
    nat semiring_no_zero_divisors);;

let monoid_mult_nat =
  ({semigroup_mult_monoid_mult = semigroup_mult_nat;
     power_monoid_mult = power_nat}
    : nat monoid_mult);;

let semiring_numeral_nat =
  ({monoid_mult_semiring_numeral = monoid_mult_nat;
     numeral_semiring_numeral = numeral_nat;
     semiring_semiring_numeral = semiring_nat}
    : nat semiring_numeral);;

let zero_neq_one_nat =
  ({one_zero_neq_one = one_nat; zero_zero_neq_one = zero_nat} :
    nat zero_neq_one);;

let semiring_1_nat =
  ({semiring_numeral_semiring_1 = semiring_numeral_nat;
     semiring_0_semiring_1 = semiring_0_nat;
     zero_neq_one_semiring_1 = zero_neq_one_nat}
    : nat semiring_1);;

let semiring_1_no_zero_divisors_nat =
  ({semiring_1_semiring_1_no_zero_divisors = semiring_1_nat;
     semiring_no_zero_divisors_semiring_1_no_zero_divisors =
       semiring_no_zero_divisors_nat}
    : nat semiring_1_no_zero_divisors);;

let cancel_semigroup_add_nat =
  ({semigroup_add_cancel_semigroup_add = semigroup_add_nat} :
    nat cancel_semigroup_add);;

let cancel_ab_semigroup_add_nat =
  ({ab_semigroup_add_cancel_ab_semigroup_add = ab_semigroup_add_nat;
     cancel_semigroup_add_cancel_ab_semigroup_add = cancel_semigroup_add_nat;
     minus_cancel_ab_semigroup_add = minus_nat}
    : nat cancel_ab_semigroup_add);;

let cancel_comm_monoid_add_nat =
  ({cancel_ab_semigroup_add_cancel_comm_monoid_add =
      cancel_ab_semigroup_add_nat;
     comm_monoid_add_cancel_comm_monoid_add = comm_monoid_add_nat}
    : nat cancel_comm_monoid_add);;

let semiring_0_cancel_nat =
  ({cancel_comm_monoid_add_semiring_0_cancel = cancel_comm_monoid_add_nat;
     semiring_0_semiring_0_cancel = semiring_0_nat}
    : nat semiring_0_cancel);;

let ab_semigroup_mult_nat =
  ({semigroup_mult_ab_semigroup_mult = semigroup_mult_nat} :
    nat ab_semigroup_mult);;

let comm_semiring_nat =
  ({ab_semigroup_mult_comm_semiring = ab_semigroup_mult_nat;
     semiring_comm_semiring = semiring_nat}
    : nat comm_semiring);;

let comm_semiring_0_nat =
  ({comm_semiring_comm_semiring_0 = comm_semiring_nat;
     semiring_0_comm_semiring_0 = semiring_0_nat}
    : nat comm_semiring_0);;

let comm_semiring_0_cancel_nat =
  ({comm_semiring_0_comm_semiring_0_cancel = comm_semiring_0_nat;
     semiring_0_cancel_comm_semiring_0_cancel = semiring_0_cancel_nat}
    : nat comm_semiring_0_cancel);;

let semiring_1_cancel_nat =
  ({semiring_0_cancel_semiring_1_cancel = semiring_0_cancel_nat;
     semiring_1_semiring_1_cancel = semiring_1_nat}
    : nat semiring_1_cancel);;

let comm_monoid_mult_nat =
  ({ab_semigroup_mult_comm_monoid_mult = ab_semigroup_mult_nat;
     monoid_mult_comm_monoid_mult = monoid_mult_nat;
     dvd_comm_monoid_mult = dvd_nat}
    : nat comm_monoid_mult);;

let comm_semiring_1_nat =
  ({comm_monoid_mult_comm_semiring_1 = comm_monoid_mult_nat;
     comm_semiring_0_comm_semiring_1 = comm_semiring_0_nat;
     semiring_1_comm_semiring_1 = semiring_1_nat}
    : nat comm_semiring_1);;

let comm_semiring_1_cancel_nat =
  ({comm_semiring_0_cancel_comm_semiring_1_cancel = comm_semiring_0_cancel_nat;
     comm_semiring_1_comm_semiring_1_cancel = comm_semiring_1_nat;
     semiring_1_cancel_comm_semiring_1_cancel = semiring_1_cancel_nat}
    : nat comm_semiring_1_cancel);;

let semidom_nat =
  ({comm_semiring_1_cancel_semidom = comm_semiring_1_cancel_nat;
     semiring_1_no_zero_divisors_semidom = semiring_1_no_zero_divisors_nat}
    : nat semidom);;

let preorder_nat = ({ord_preorder = ord_nat} : nat preorder);;

let order_nat = ({preorder_order = preorder_nat} : nat order);;

type 'a semilattice_sup =
  {sup_semilattice_sup : 'a sup; order_semilattice_sup : 'a order};;

type 'a semilattice_inf =
  {inf_semilattice_inf : 'a inf; order_semilattice_inf : 'a order};;

type 'a lattice =
  {semilattice_inf_lattice : 'a semilattice_inf;
    semilattice_sup_lattice : 'a semilattice_sup};;

let semilattice_sup_nat =
  ({sup_semilattice_sup = sup_nat; order_semilattice_sup = order_nat} :
    nat semilattice_sup);;

let semilattice_inf_nat =
  ({inf_semilattice_inf = inf_nat; order_semilattice_inf = order_nat} :
    nat semilattice_inf);;

let lattice_nat =
  ({semilattice_inf_lattice = semilattice_inf_nat;
     semilattice_sup_lattice = semilattice_sup_nat}
    : nat lattice);;

let ceq_nata : (nat -> nat -> bool) option = Some equal_nata;;

let ceq_nat = ({ceq = ceq_nata} : nat ceq);;

let set_impl_nata : (nat, set_impla) phantom = Phantom Set_RBT;;

let set_impl_nat = ({set_impl = set_impl_nata} : nat set_impl);;

let linorder_nat = ({order_linorder = order_nat} : nat linorder);;

type 'a semiring_char_0 = {semiring_1_semiring_char_0 : 'a semiring_1};;

let semiring_char_0_nat =
  ({semiring_1_semiring_char_0 = semiring_1_nat} : nat semiring_char_0);;

let semiring_no_zero_divisors_cancel_nat =
  ({semiring_no_zero_divisors_semiring_no_zero_divisors_cancel =
      semiring_no_zero_divisors_nat}
    : nat semiring_no_zero_divisors_cancel);;

let semidom_divide_nat =
  ({divide_semidom_divide = divide_nat; semidom_semidom_divide = semidom_nat;
     semiring_no_zero_divisors_cancel_semidom_divide =
       semiring_no_zero_divisors_cancel_nat}
    : nat semidom_divide);;

let algebraic_semidom_nat =
  ({semidom_divide_algebraic_semidom = semidom_divide_nat} :
    nat algebraic_semidom);;

let semiring_modulo_nat =
  ({comm_semiring_1_cancel_semiring_modulo = comm_semiring_1_cancel_nat;
     modulo_semiring_modulo = modulo_nat}
    : nat semiring_modulo);;

let semidom_modulo_nat =
  ({algebraic_semidom_semidom_modulo = algebraic_semidom_nat;
     semiring_modulo_semidom_modulo = semiring_modulo_nat}
    : nat semidom_modulo);;

let cEnum_nat :
  (nat list * (((nat -> bool) -> bool) * ((nat -> bool) -> bool))) option
  = None;;

type 'a cenum =
  {cEnum :
     ('a list * ((('a -> bool) -> bool) * (('a -> bool) -> bool))) option};;
let cEnum _A = _A.cEnum;;

let cenum_nat = ({cEnum = cEnum_nat} : nat cenum);;

let finite_UNIV_nata : (nat, bool) phantom = Phantom false;;

type 'a finite_UNIV = {finite_UNIV : ('a, bool) phantom};;
let finite_UNIV _A = _A.finite_UNIV;;

let finite_UNIV_nat = ({finite_UNIV = finite_UNIV_nata} : nat finite_UNIV);;

let rec compare_nat x = comparator_of (equal_nat, linorder_nat) x;;

let ccompare_nata : (nat -> nat -> ordera) option = Some compare_nat;;

let ccompare_nat = ({ccompare = ccompare_nata} : nat ccompare);;

let rec proper_interval_nat
  no x1 = match no, x1 with no, None -> true
    | None, Some x -> less_nat zero_nata x
    | Some x, Some y -> less_nat one_nata (minus_nata y x);;

let rec cproper_interval_nata x = proper_interval_nat x;;

type 'a cproper_interval =
  {ccompare_cproper_interval : 'a ccompare;
    cproper_interval : 'a option -> 'a option -> bool};;
let cproper_interval _A = _A.cproper_interval;;

let cproper_interval_nat =
  ({ccompare_cproper_interval = ccompare_nat;
     cproper_interval = cproper_interval_nata}
    : nat cproper_interval);;

let rec equal_prod _A _B (x1, x2) (y1, y2) = eq _A x1 y1 && eq _B x2 y2;;

type rat = Frct of (int * int);;

let rec quotient_of (Frct x) = x;;

let rec equal_rata
  a b = equal_prod equal_int equal_int (quotient_of a) (quotient_of b);;

let equal_rat = ({equal = equal_rata} : rat equal);;

let rec normalize
  p = (if less_int zero_inta (snd p)
        then (let a = gcd_intc (fst p) (snd p) in
               (divide_inta (fst p) a, divide_inta (snd p) a))
        else (if equal_inta (snd p) zero_inta then (zero_inta, one_inta)
               else (let a = uminus_inta (gcd_intc (fst p) (snd p)) in
                      (divide_inta (fst p) a, divide_inta (snd p) a))));;

let rec times_rata
  p q = Frct (let (a, c) = quotient_of p in
              let (b, d) = quotient_of q in
               normalize (times_inta a b, times_inta c d));;

let times_rat = ({times = times_rata} : rat times);;

let dvd_rat = ({times_dvd = times_rat} : rat dvd);;

let rec abs_rata p = Frct (let (a, b) = quotient_of p in (abs_int a, b));;

type 'a abs = {abs : 'a -> 'a};;
let abs _A = _A.abs;;

let abs_rat = ({abs = abs_rata} : rat abs);;

let one_rata : rat = Frct (one_inta, one_inta);;

let one_rat = ({one = one_rata} : rat one);;

let rec sgn_rata p = Frct (sgn_int (fst (quotient_of p)), one_inta);;

type 'a sgn = {sgn : 'a -> 'a};;
let sgn _A = _A.sgn;;

let sgn_rat = ({sgn = sgn_rata} : rat sgn);;

let rec uminus_rata
  p = Frct (let (a, b) = quotient_of p in (uminus_inta a, b));;

let rec minus_rata
  p q = Frct (let (a, c) = quotient_of p in
              let (b, d) = quotient_of q in
               normalize
                 (minus_inta (times_inta a d) (times_inta b c),
                   times_inta c d));;

let zero_rata : rat = Frct (zero_inta, one_inta);;

let rec plus_rata
  p q = Frct (let (a, c) = quotient_of p in
              let (b, d) = quotient_of q in
               normalize
                 (plus_inta (times_inta a d) (times_inta b c),
                   times_inta c d));;

let plus_rat = ({plus = plus_rata} : rat plus);;

let semigroup_add_rat = ({plus_semigroup_add = plus_rat} : rat semigroup_add);;

let cancel_semigroup_add_rat =
  ({semigroup_add_cancel_semigroup_add = semigroup_add_rat} :
    rat cancel_semigroup_add);;

let ab_semigroup_add_rat =
  ({semigroup_add_ab_semigroup_add = semigroup_add_rat} :
    rat ab_semigroup_add);;

let minus_rat = ({minus = minus_rata} : rat minus);;

let cancel_ab_semigroup_add_rat =
  ({ab_semigroup_add_cancel_ab_semigroup_add = ab_semigroup_add_rat;
     cancel_semigroup_add_cancel_ab_semigroup_add = cancel_semigroup_add_rat;
     minus_cancel_ab_semigroup_add = minus_rat}
    : rat cancel_ab_semigroup_add);;

let zero_rat = ({zero = zero_rata} : rat zero);;

let monoid_add_rat =
  ({semigroup_add_monoid_add = semigroup_add_rat; zero_monoid_add = zero_rat} :
    rat monoid_add);;

let comm_monoid_add_rat =
  ({ab_semigroup_add_comm_monoid_add = ab_semigroup_add_rat;
     monoid_add_comm_monoid_add = monoid_add_rat}
    : rat comm_monoid_add);;

let cancel_comm_monoid_add_rat =
  ({cancel_ab_semigroup_add_cancel_comm_monoid_add =
      cancel_ab_semigroup_add_rat;
     comm_monoid_add_cancel_comm_monoid_add = comm_monoid_add_rat}
    : rat cancel_comm_monoid_add);;

let mult_zero_rat =
  ({times_mult_zero = times_rat; zero_mult_zero = zero_rat} : rat mult_zero);;

let semigroup_mult_rat =
  ({times_semigroup_mult = times_rat} : rat semigroup_mult);;

let semiring_rat =
  ({ab_semigroup_add_semiring = ab_semigroup_add_rat;
     semigroup_mult_semiring = semigroup_mult_rat}
    : rat semiring);;

let semiring_0_rat =
  ({comm_monoid_add_semiring_0 = comm_monoid_add_rat;
     mult_zero_semiring_0 = mult_zero_rat; semiring_semiring_0 = semiring_rat}
    : rat semiring_0);;

let semiring_0_cancel_rat =
  ({cancel_comm_monoid_add_semiring_0_cancel = cancel_comm_monoid_add_rat;
     semiring_0_semiring_0_cancel = semiring_0_rat}
    : rat semiring_0_cancel);;

let ab_semigroup_mult_rat =
  ({semigroup_mult_ab_semigroup_mult = semigroup_mult_rat} :
    rat ab_semigroup_mult);;

let comm_semiring_rat =
  ({ab_semigroup_mult_comm_semiring = ab_semigroup_mult_rat;
     semiring_comm_semiring = semiring_rat}
    : rat comm_semiring);;

let comm_semiring_0_rat =
  ({comm_semiring_comm_semiring_0 = comm_semiring_rat;
     semiring_0_comm_semiring_0 = semiring_0_rat}
    : rat comm_semiring_0);;

let comm_semiring_0_cancel_rat =
  ({comm_semiring_0_comm_semiring_0_cancel = comm_semiring_0_rat;
     semiring_0_cancel_comm_semiring_0_cancel = semiring_0_cancel_rat}
    : rat comm_semiring_0_cancel);;

let power_rat = ({one_power = one_rat; times_power = times_rat} : rat power);;

let monoid_mult_rat =
  ({semigroup_mult_monoid_mult = semigroup_mult_rat;
     power_monoid_mult = power_rat}
    : rat monoid_mult);;

let numeral_rat =
  ({one_numeral = one_rat; semigroup_add_numeral = semigroup_add_rat} :
    rat numeral);;

let semiring_numeral_rat =
  ({monoid_mult_semiring_numeral = monoid_mult_rat;
     numeral_semiring_numeral = numeral_rat;
     semiring_semiring_numeral = semiring_rat}
    : rat semiring_numeral);;

let zero_neq_one_rat =
  ({one_zero_neq_one = one_rat; zero_zero_neq_one = zero_rat} :
    rat zero_neq_one);;

let semiring_1_rat =
  ({semiring_numeral_semiring_1 = semiring_numeral_rat;
     semiring_0_semiring_1 = semiring_0_rat;
     zero_neq_one_semiring_1 = zero_neq_one_rat}
    : rat semiring_1);;

let semiring_1_cancel_rat =
  ({semiring_0_cancel_semiring_1_cancel = semiring_0_cancel_rat;
     semiring_1_semiring_1_cancel = semiring_1_rat}
    : rat semiring_1_cancel);;

let comm_monoid_mult_rat =
  ({ab_semigroup_mult_comm_monoid_mult = ab_semigroup_mult_rat;
     monoid_mult_comm_monoid_mult = monoid_mult_rat;
     dvd_comm_monoid_mult = dvd_rat}
    : rat comm_monoid_mult);;

let comm_semiring_1_rat =
  ({comm_monoid_mult_comm_semiring_1 = comm_monoid_mult_rat;
     comm_semiring_0_comm_semiring_1 = comm_semiring_0_rat;
     semiring_1_comm_semiring_1 = semiring_1_rat}
    : rat comm_semiring_1);;

let comm_semiring_1_cancel_rat =
  ({comm_semiring_0_cancel_comm_semiring_1_cancel = comm_semiring_0_cancel_rat;
     comm_semiring_1_comm_semiring_1_cancel = comm_semiring_1_rat;
     semiring_1_cancel_comm_semiring_1_cancel = semiring_1_cancel_rat}
    : rat comm_semiring_1_cancel);;

let comm_semiring_1_cancel_crossproduct_rat =
  ({comm_semiring_1_cancel_comm_semiring_1_cancel_crossproduct =
      comm_semiring_1_cancel_rat}
    : rat comm_semiring_1_cancel_crossproduct);;

let semiring_no_zero_divisors_rat =
  ({semiring_0_semiring_no_zero_divisors = semiring_0_rat} :
    rat semiring_no_zero_divisors);;

let semiring_1_no_zero_divisors_rat =
  ({semiring_1_semiring_1_no_zero_divisors = semiring_1_rat;
     semiring_no_zero_divisors_semiring_1_no_zero_divisors =
       semiring_no_zero_divisors_rat}
    : rat semiring_1_no_zero_divisors);;

let semiring_no_zero_divisors_cancel_rat =
  ({semiring_no_zero_divisors_semiring_no_zero_divisors_cancel =
      semiring_no_zero_divisors_rat}
    : rat semiring_no_zero_divisors_cancel);;

let uminus_rat = ({uminus = uminus_rata} : rat uminus);;

let group_add_rat =
  ({cancel_semigroup_add_group_add = cancel_semigroup_add_rat;
     minus_group_add = minus_rat; monoid_add_group_add = monoid_add_rat;
     uminus_group_add = uminus_rat}
    : rat group_add);;

let ab_group_add_rat =
  ({cancel_comm_monoid_add_ab_group_add = cancel_comm_monoid_add_rat;
     group_add_ab_group_add = group_add_rat}
    : rat ab_group_add);;

let ring_rat =
  ({ab_group_add_ring = ab_group_add_rat;
     semiring_0_cancel_ring = semiring_0_cancel_rat}
    : rat ring);;

let ring_no_zero_divisors_rat =
  ({ring_ring_no_zero_divisors = ring_rat;
     semiring_no_zero_divisors_cancel_ring_no_zero_divisors =
       semiring_no_zero_divisors_cancel_rat}
    : rat ring_no_zero_divisors);;

let neg_numeral_rat =
  ({group_add_neg_numeral = group_add_rat; numeral_neg_numeral = numeral_rat} :
    rat neg_numeral);;

let ring_1_rat =
  ({neg_numeral_ring_1 = neg_numeral_rat; ring_ring_1 = ring_rat;
     semiring_1_cancel_ring_1 = semiring_1_cancel_rat}
    : rat ring_1);;

let ring_1_no_zero_divisors_rat =
  ({ring_1_ring_1_no_zero_divisors = ring_1_rat;
     ring_no_zero_divisors_ring_1_no_zero_divisors = ring_no_zero_divisors_rat;
     semiring_1_no_zero_divisors_ring_1_no_zero_divisors =
       semiring_1_no_zero_divisors_rat}
    : rat ring_1_no_zero_divisors);;

let comm_ring_rat =
  ({comm_semiring_0_cancel_comm_ring = comm_semiring_0_cancel_rat;
     ring_comm_ring = ring_rat}
    : rat comm_ring);;

let comm_ring_1_rat =
  ({comm_ring_comm_ring_1 = comm_ring_rat;
     comm_semiring_1_cancel_comm_ring_1 = comm_semiring_1_cancel_rat;
     ring_1_comm_ring_1 = ring_1_rat}
    : rat comm_ring_1);;

let semidom_rat =
  ({comm_semiring_1_cancel_semidom = comm_semiring_1_cancel_rat;
     semiring_1_no_zero_divisors_semidom = semiring_1_no_zero_divisors_rat}
    : rat semidom);;

let idom_rat =
  ({comm_ring_1_idom = comm_ring_1_rat;
     ring_1_no_zero_divisors_idom = ring_1_no_zero_divisors_rat;
     semidom_idom = semidom_rat;
     comm_semiring_1_cancel_crossproduct_idom =
       comm_semiring_1_cancel_crossproduct_rat}
    : rat idom);;

let rec inverse_rata
  p = Frct (let (a, b) = quotient_of p in
             (if equal_inta a zero_inta then (zero_inta, one_inta)
               else (times_inta (sgn_int a) b, abs_int a)));;

let rec divide_rata
  p q = Frct (let (a, c) = quotient_of p in
              let (b, d) = quotient_of q in
               normalize (times_inta a d, times_inta c b));;

type 'a inverse = {divide_inverse : 'a divide; inverse : 'a -> 'a};;
let inverse _A = _A.inverse;;

type 'a ufd = {idom_ufd : 'a idom};;

type 'a division_ring =
  {inverse_division_ring : 'a inverse;
    ring_1_no_zero_divisors_division_ring : 'a ring_1_no_zero_divisors};;

type 'a field =
  {division_ring_field : 'a division_ring; idom_divide_field : 'a idom_divide;
    ufd_field : 'a ufd};;

let ufd_rat = ({idom_ufd = idom_rat} : rat ufd);;

let divide_rat = ({divide = divide_rata} : rat divide);;

let inverse_rat =
  ({divide_inverse = divide_rat; inverse = inverse_rata} : rat inverse);;

let division_ring_rat =
  ({inverse_division_ring = inverse_rat;
     ring_1_no_zero_divisors_division_ring = ring_1_no_zero_divisors_rat}
    : rat division_ring);;

let semidom_divide_rat =
  ({divide_semidom_divide = divide_rat; semidom_semidom_divide = semidom_rat;
     semiring_no_zero_divisors_cancel_semidom_divide =
       semiring_no_zero_divisors_cancel_rat}
    : rat semidom_divide);;

let idom_divide_rat =
  ({idom_idom_divide = idom_rat;
     semidom_divide_idom_divide = semidom_divide_rat}
    : rat idom_divide);;

let field_rat =
  ({division_ring_field = division_ring_rat;
     idom_divide_field = idom_divide_rat; ufd_field = ufd_rat}
    : rat field);;

let rec less_eq_rat
  p q = (let (a, c) = quotient_of p in
         let (b, d) = quotient_of q in
          less_eq_int (times_inta a d) (times_inta c b));;

let rec less_rat
  p q = (let (a, c) = quotient_of p in
         let (b, d) = quotient_of q in
          less_int (times_inta a d) (times_inta c b));;

type 'a abs_if =
  {abs_abs_if : 'a abs; minus_abs_if : 'a minus; uminus_abs_if : 'a uminus;
    zero_abs_if : 'a zero; ord_abs_if : 'a ord};;

let ord_rat = ({less_eq = less_eq_rat; less = less_rat} : rat ord);;

let abs_if_rat =
  ({abs_abs_if = abs_rat; minus_abs_if = minus_rat; uminus_abs_if = uminus_rat;
     zero_abs_if = zero_rat; ord_abs_if = ord_rat}
    : rat abs_if);;

type 'a ring_char_0 =
  {semiring_char_0_ring_char_0 : 'a semiring_char_0;
    ring_1_ring_char_0 : 'a ring_1};;

let semiring_char_0_rat =
  ({semiring_1_semiring_char_0 = semiring_1_rat} : rat semiring_char_0);;

let ring_char_0_rat =
  ({semiring_char_0_ring_char_0 = semiring_char_0_rat;
     ring_1_ring_char_0 = ring_1_rat}
    : rat ring_char_0);;

let preorder_rat = ({ord_preorder = ord_rat} : rat preorder);;

let order_rat = ({preorder_order = preorder_rat} : rat order);;

type 'a no_bot = {order_no_bot : 'a order};;

let no_bot_rat = ({order_no_bot = order_rat} : rat no_bot);;

type 'a no_top = {order_no_top : 'a order};;

let no_top_rat = ({order_no_top = order_rat} : rat no_top);;

let linorder_rat = ({order_linorder = order_rat} : rat linorder);;

type 'a idom_abs_sgn =
  {abs_idom_abs_sgn : 'a abs; sgn_idom_abs_sgn : 'a sgn;
    idom_idom_abs_sgn : 'a idom};;

let idom_abs_sgn_rat =
  ({abs_idom_abs_sgn = abs_rat; sgn_idom_abs_sgn = sgn_rat;
     idom_idom_abs_sgn = idom_rat}
    : rat idom_abs_sgn);;

type 'a ordered_ab_semigroup_add =
  {ab_semigroup_add_ordered_ab_semigroup_add : 'a ab_semigroup_add;
    order_ordered_ab_semigroup_add : 'a order};;

type 'a strict_ordered_ab_semigroup_add =
  {ordered_ab_semigroup_add_strict_ordered_ab_semigroup_add :
     'a ordered_ab_semigroup_add};;

type 'a ordered_cancel_ab_semigroup_add =
  {cancel_ab_semigroup_add_ordered_cancel_ab_semigroup_add :
     'a cancel_ab_semigroup_add;
    strict_ordered_ab_semigroup_add_ordered_cancel_ab_semigroup_add :
      'a strict_ordered_ab_semigroup_add};;

type 'a ordered_comm_monoid_add =
  {comm_monoid_add_ordered_comm_monoid_add : 'a comm_monoid_add;
    ordered_ab_semigroup_add_ordered_comm_monoid_add :
      'a ordered_ab_semigroup_add};;

type 'a ordered_semiring =
  {ordered_comm_monoid_add_ordered_semiring : 'a ordered_comm_monoid_add;
    semiring_ordered_semiring : 'a semiring};;

type 'a ordered_semiring_0 =
  {ordered_semiring_ordered_semiring_0 : 'a ordered_semiring;
    semiring_0_ordered_semiring_0 : 'a semiring_0};;

type 'a ordered_cancel_semiring =
  {ordered_cancel_ab_semigroup_add_ordered_cancel_semiring :
     'a ordered_cancel_ab_semigroup_add;
    ordered_semiring_0_ordered_cancel_semiring : 'a ordered_semiring_0;
    semiring_0_cancel_ordered_cancel_semiring : 'a semiring_0_cancel};;

type 'a ordered_ab_semigroup_add_imp_le =
  {ordered_cancel_ab_semigroup_add_ordered_ab_semigroup_add_imp_le :
     'a ordered_cancel_ab_semigroup_add};;

type 'a strict_ordered_comm_monoid_add =
  {comm_monoid_add_strict_ordered_comm_monoid_add : 'a comm_monoid_add;
    strict_ordered_ab_semigroup_add_strict_ordered_comm_monoid_add :
      'a strict_ordered_ab_semigroup_add};;

type 'a ordered_cancel_comm_monoid_add =
  {ordered_cancel_ab_semigroup_add_ordered_cancel_comm_monoid_add :
     'a ordered_cancel_ab_semigroup_add;
    ordered_comm_monoid_add_ordered_cancel_comm_monoid_add :
      'a ordered_comm_monoid_add;
    strict_ordered_comm_monoid_add_ordered_cancel_comm_monoid_add :
      'a strict_ordered_comm_monoid_add};;

type 'a ordered_ab_semigroup_monoid_add_imp_le =
  {cancel_comm_monoid_add_ordered_ab_semigroup_monoid_add_imp_le :
     'a cancel_comm_monoid_add;
    ordered_ab_semigroup_add_imp_le_ordered_ab_semigroup_monoid_add_imp_le :
      'a ordered_ab_semigroup_add_imp_le;
    ordered_cancel_comm_monoid_add_ordered_ab_semigroup_monoid_add_imp_le :
      'a ordered_cancel_comm_monoid_add};;

type 'a ordered_ab_group_add =
  {ab_group_add_ordered_ab_group_add : 'a ab_group_add;
    ordered_ab_semigroup_monoid_add_imp_le_ordered_ab_group_add :
      'a ordered_ab_semigroup_monoid_add_imp_le};;

type 'a ordered_ring =
  {ordered_ab_group_add_ordered_ring : 'a ordered_ab_group_add;
    ordered_cancel_semiring_ordered_ring : 'a ordered_cancel_semiring;
    ring_ordered_ring : 'a ring};;

let ordered_ab_semigroup_add_rat =
  ({ab_semigroup_add_ordered_ab_semigroup_add = ab_semigroup_add_rat;
     order_ordered_ab_semigroup_add = order_rat}
    : rat ordered_ab_semigroup_add);;

let strict_ordered_ab_semigroup_add_rat =
  ({ordered_ab_semigroup_add_strict_ordered_ab_semigroup_add =
      ordered_ab_semigroup_add_rat}
    : rat strict_ordered_ab_semigroup_add);;

let ordered_cancel_ab_semigroup_add_rat =
  ({cancel_ab_semigroup_add_ordered_cancel_ab_semigroup_add =
      cancel_ab_semigroup_add_rat;
     strict_ordered_ab_semigroup_add_ordered_cancel_ab_semigroup_add =
       strict_ordered_ab_semigroup_add_rat}
    : rat ordered_cancel_ab_semigroup_add);;

let ordered_comm_monoid_add_rat =
  ({comm_monoid_add_ordered_comm_monoid_add = comm_monoid_add_rat;
     ordered_ab_semigroup_add_ordered_comm_monoid_add =
       ordered_ab_semigroup_add_rat}
    : rat ordered_comm_monoid_add);;

let ordered_semiring_rat =
  ({ordered_comm_monoid_add_ordered_semiring = ordered_comm_monoid_add_rat;
     semiring_ordered_semiring = semiring_rat}
    : rat ordered_semiring);;

let ordered_semiring_0_rat =
  ({ordered_semiring_ordered_semiring_0 = ordered_semiring_rat;
     semiring_0_ordered_semiring_0 = semiring_0_rat}
    : rat ordered_semiring_0);;

let ordered_cancel_semiring_rat =
  ({ordered_cancel_ab_semigroup_add_ordered_cancel_semiring =
      ordered_cancel_ab_semigroup_add_rat;
     ordered_semiring_0_ordered_cancel_semiring = ordered_semiring_0_rat;
     semiring_0_cancel_ordered_cancel_semiring = semiring_0_cancel_rat}
    : rat ordered_cancel_semiring);;

let ordered_ab_semigroup_add_imp_le_rat =
  ({ordered_cancel_ab_semigroup_add_ordered_ab_semigroup_add_imp_le =
      ordered_cancel_ab_semigroup_add_rat}
    : rat ordered_ab_semigroup_add_imp_le);;

let strict_ordered_comm_monoid_add_rat =
  ({comm_monoid_add_strict_ordered_comm_monoid_add = comm_monoid_add_rat;
     strict_ordered_ab_semigroup_add_strict_ordered_comm_monoid_add =
       strict_ordered_ab_semigroup_add_rat}
    : rat strict_ordered_comm_monoid_add);;

let ordered_cancel_comm_monoid_add_rat =
  ({ordered_cancel_ab_semigroup_add_ordered_cancel_comm_monoid_add =
      ordered_cancel_ab_semigroup_add_rat;
     ordered_comm_monoid_add_ordered_cancel_comm_monoid_add =
       ordered_comm_monoid_add_rat;
     strict_ordered_comm_monoid_add_ordered_cancel_comm_monoid_add =
       strict_ordered_comm_monoid_add_rat}
    : rat ordered_cancel_comm_monoid_add);;

let ordered_ab_semigroup_monoid_add_imp_le_rat =
  ({cancel_comm_monoid_add_ordered_ab_semigroup_monoid_add_imp_le =
      cancel_comm_monoid_add_rat;
     ordered_ab_semigroup_add_imp_le_ordered_ab_semigroup_monoid_add_imp_le =
       ordered_ab_semigroup_add_imp_le_rat;
     ordered_cancel_comm_monoid_add_ordered_ab_semigroup_monoid_add_imp_le =
       ordered_cancel_comm_monoid_add_rat}
    : rat ordered_ab_semigroup_monoid_add_imp_le);;

let ordered_ab_group_add_rat =
  ({ab_group_add_ordered_ab_group_add = ab_group_add_rat;
     ordered_ab_semigroup_monoid_add_imp_le_ordered_ab_group_add =
       ordered_ab_semigroup_monoid_add_imp_le_rat}
    : rat ordered_ab_group_add);;

let ordered_ring_rat =
  ({ordered_ab_group_add_ordered_ring = ordered_ab_group_add_rat;
     ordered_cancel_semiring_ordered_ring = ordered_cancel_semiring_rat;
     ring_ordered_ring = ring_rat}
    : rat ordered_ring);;

type 'a field_char_0 =
  {field_field_char_0 : 'a field; ring_char_0_field_char_0 : 'a ring_char_0};;

let field_char_0_rat =
  ({field_field_char_0 = field_rat; ring_char_0_field_char_0 = ring_char_0_rat}
    : rat field_char_0);;

type 'a zero_less_one =
  {order_zero_less_one : 'a order;
    zero_neq_one_zero_less_one : 'a zero_neq_one};;

let zero_less_one_rat =
  ({order_zero_less_one = order_rat;
     zero_neq_one_zero_less_one = zero_neq_one_rat}
    : rat zero_less_one);;

type 'a field_abs_sgn =
  {field_field_abs_sgn : 'a field;
    idom_abs_sgn_field_abs_sgn : 'a idom_abs_sgn};;

let field_abs_sgn_rat =
  ({field_field_abs_sgn = field_rat;
     idom_abs_sgn_field_abs_sgn = idom_abs_sgn_rat}
    : rat field_abs_sgn);;

type 'a dense_order = {order_dense_order : 'a order};;

let dense_order_rat = ({order_dense_order = order_rat} : rat dense_order);;

type 'a linordered_ab_semigroup_add =
  {ordered_ab_semigroup_add_linordered_ab_semigroup_add :
     'a ordered_ab_semigroup_add;
    linorder_linordered_ab_semigroup_add : 'a linorder};;

type 'a linordered_cancel_ab_semigroup_add =
  {linordered_ab_semigroup_add_linordered_cancel_ab_semigroup_add :
     'a linordered_ab_semigroup_add;
    ordered_ab_semigroup_add_imp_le_linordered_cancel_ab_semigroup_add :
      'a ordered_ab_semigroup_add_imp_le};;

type 'a linordered_semiring =
  {linordered_cancel_ab_semigroup_add_linordered_semiring :
     'a linordered_cancel_ab_semigroup_add;
    ordered_ab_semigroup_monoid_add_imp_le_linordered_semiring :
      'a ordered_ab_semigroup_monoid_add_imp_le;
    ordered_cancel_semiring_linordered_semiring : 'a ordered_cancel_semiring};;

type 'a linordered_semiring_strict =
  {linordered_semiring_linordered_semiring_strict : 'a linordered_semiring};;

type 'a linordered_semiring_1 =
  {linordered_semiring_linordered_semiring_1 : 'a linordered_semiring;
    semiring_1_linordered_semiring_1 : 'a semiring_1;
    zero_less_one_linordered_semiring_1 : 'a zero_less_one};;

type 'a linordered_semiring_1_strict =
  {linordered_semiring_1_linordered_semiring_1_strict :
     'a linordered_semiring_1;
    linordered_semiring_strict_linordered_semiring_1_strict :
      'a linordered_semiring_strict};;

type 'a ordered_ab_group_add_abs =
  {abs_ordered_ab_group_add_abs : 'a abs;
    ordered_ab_group_add_ordered_ab_group_add_abs : 'a ordered_ab_group_add};;

type 'a linordered_ab_group_add =
  {linordered_cancel_ab_semigroup_add_linordered_ab_group_add :
     'a linordered_cancel_ab_semigroup_add;
    ordered_ab_group_add_linordered_ab_group_add : 'a ordered_ab_group_add};;

type 'a linordered_ring =
  {linordered_ab_group_add_linordered_ring : 'a linordered_ab_group_add;
    ordered_ab_group_add_abs_linordered_ring : 'a ordered_ab_group_add_abs;
    abs_if_linordered_ring : 'a abs_if;
    linordered_semiring_linordered_ring : 'a linordered_semiring;
    ordered_ring_linordered_ring : 'a ordered_ring};;

type 'a linordered_ring_strict =
  {linordered_ring_linordered_ring_strict : 'a linordered_ring;
    linordered_semiring_strict_linordered_ring_strict :
      'a linordered_semiring_strict;
    ring_no_zero_divisors_linordered_ring_strict : 'a ring_no_zero_divisors};;

type 'a ordered_comm_semiring =
  {comm_semiring_0_ordered_comm_semiring : 'a comm_semiring_0;
    ordered_semiring_ordered_comm_semiring : 'a ordered_semiring};;

type 'a ordered_cancel_comm_semiring =
  {comm_semiring_0_cancel_ordered_cancel_comm_semiring :
     'a comm_semiring_0_cancel;
    ordered_cancel_semiring_ordered_cancel_comm_semiring :
      'a ordered_cancel_semiring;
    ordered_comm_semiring_ordered_cancel_comm_semiring :
      'a ordered_comm_semiring};;

type 'a linordered_comm_semiring_strict =
  {linordered_semiring_strict_linordered_comm_semiring_strict :
     'a linordered_semiring_strict;
    ordered_cancel_comm_semiring_linordered_comm_semiring_strict :
      'a ordered_cancel_comm_semiring};;

type 'a linordered_nonzero_semiring =
  {semiring_char_0_linordered_nonzero_semiring : 'a semiring_char_0;
    linorder_linordered_nonzero_semiring : 'a linorder;
    comm_semiring_1_linordered_nonzero_semiring : 'a comm_semiring_1;
    ordered_comm_semiring_linordered_nonzero_semiring :
      'a ordered_comm_semiring;
    zero_less_one_linordered_nonzero_semiring : 'a zero_less_one};;

type 'a linordered_semidom =
  {linordered_comm_semiring_strict_linordered_semidom :
     'a linordered_comm_semiring_strict;
    linordered_nonzero_semiring_linordered_semidom :
      'a linordered_nonzero_semiring;
    semidom_linordered_semidom : 'a semidom};;

type 'a ordered_comm_ring =
  {comm_ring_ordered_comm_ring : 'a comm_ring;
    ordered_cancel_comm_semiring_ordered_comm_ring :
      'a ordered_cancel_comm_semiring;
    ordered_ring_ordered_comm_ring : 'a ordered_ring};;

type 'a ordered_ring_abs =
  {ordered_ab_group_add_abs_ordered_ring_abs : 'a ordered_ab_group_add_abs;
    ordered_ring_ordered_ring_abs : 'a ordered_ring};;

type 'a linordered_idom =
  {ring_char_0_linordered_idom : 'a ring_char_0;
    idom_abs_sgn_linordered_idom : 'a idom_abs_sgn;
    linordered_ring_strict_linordered_idom : 'a linordered_ring_strict;
    linordered_semidom_linordered_idom : 'a linordered_semidom;
    linordered_semiring_1_strict_linordered_idom :
      'a linordered_semiring_1_strict;
    ordered_comm_ring_linordered_idom : 'a ordered_comm_ring;
    ordered_ring_abs_linordered_idom : 'a ordered_ring_abs};;

let linordered_ab_semigroup_add_rat =
  ({ordered_ab_semigroup_add_linordered_ab_semigroup_add =
      ordered_ab_semigroup_add_rat;
     linorder_linordered_ab_semigroup_add = linorder_rat}
    : rat linordered_ab_semigroup_add);;

let linordered_cancel_ab_semigroup_add_rat =
  ({linordered_ab_semigroup_add_linordered_cancel_ab_semigroup_add =
      linordered_ab_semigroup_add_rat;
     ordered_ab_semigroup_add_imp_le_linordered_cancel_ab_semigroup_add =
       ordered_ab_semigroup_add_imp_le_rat}
    : rat linordered_cancel_ab_semigroup_add);;

let linordered_semiring_rat =
  ({linordered_cancel_ab_semigroup_add_linordered_semiring =
      linordered_cancel_ab_semigroup_add_rat;
     ordered_ab_semigroup_monoid_add_imp_le_linordered_semiring =
       ordered_ab_semigroup_monoid_add_imp_le_rat;
     ordered_cancel_semiring_linordered_semiring = ordered_cancel_semiring_rat}
    : rat linordered_semiring);;

let linordered_semiring_strict_rat =
  ({linordered_semiring_linordered_semiring_strict = linordered_semiring_rat} :
    rat linordered_semiring_strict);;

let linordered_semiring_1_rat =
  ({linordered_semiring_linordered_semiring_1 = linordered_semiring_rat;
     semiring_1_linordered_semiring_1 = semiring_1_rat;
     zero_less_one_linordered_semiring_1 = zero_less_one_rat}
    : rat linordered_semiring_1);;

let linordered_semiring_1_strict_rat =
  ({linordered_semiring_1_linordered_semiring_1_strict =
      linordered_semiring_1_rat;
     linordered_semiring_strict_linordered_semiring_1_strict =
       linordered_semiring_strict_rat}
    : rat linordered_semiring_1_strict);;

let ordered_ab_group_add_abs_rat =
  ({abs_ordered_ab_group_add_abs = abs_rat;
     ordered_ab_group_add_ordered_ab_group_add_abs = ordered_ab_group_add_rat}
    : rat ordered_ab_group_add_abs);;

let linordered_ab_group_add_rat =
  ({linordered_cancel_ab_semigroup_add_linordered_ab_group_add =
      linordered_cancel_ab_semigroup_add_rat;
     ordered_ab_group_add_linordered_ab_group_add = ordered_ab_group_add_rat}
    : rat linordered_ab_group_add);;

let linordered_ring_rat =
  ({linordered_ab_group_add_linordered_ring = linordered_ab_group_add_rat;
     ordered_ab_group_add_abs_linordered_ring = ordered_ab_group_add_abs_rat;
     abs_if_linordered_ring = abs_if_rat;
     linordered_semiring_linordered_ring = linordered_semiring_rat;
     ordered_ring_linordered_ring = ordered_ring_rat}
    : rat linordered_ring);;

let linordered_ring_strict_rat =
  ({linordered_ring_linordered_ring_strict = linordered_ring_rat;
     linordered_semiring_strict_linordered_ring_strict =
       linordered_semiring_strict_rat;
     ring_no_zero_divisors_linordered_ring_strict = ring_no_zero_divisors_rat}
    : rat linordered_ring_strict);;

let ordered_comm_semiring_rat =
  ({comm_semiring_0_ordered_comm_semiring = comm_semiring_0_rat;
     ordered_semiring_ordered_comm_semiring = ordered_semiring_rat}
    : rat ordered_comm_semiring);;

let ordered_cancel_comm_semiring_rat =
  ({comm_semiring_0_cancel_ordered_cancel_comm_semiring =
      comm_semiring_0_cancel_rat;
     ordered_cancel_semiring_ordered_cancel_comm_semiring =
       ordered_cancel_semiring_rat;
     ordered_comm_semiring_ordered_cancel_comm_semiring =
       ordered_comm_semiring_rat}
    : rat ordered_cancel_comm_semiring);;

let linordered_comm_semiring_strict_rat =
  ({linordered_semiring_strict_linordered_comm_semiring_strict =
      linordered_semiring_strict_rat;
     ordered_cancel_comm_semiring_linordered_comm_semiring_strict =
       ordered_cancel_comm_semiring_rat}
    : rat linordered_comm_semiring_strict);;

let linordered_nonzero_semiring_rat =
  ({semiring_char_0_linordered_nonzero_semiring = semiring_char_0_rat;
     linorder_linordered_nonzero_semiring = linorder_rat;
     comm_semiring_1_linordered_nonzero_semiring = comm_semiring_1_rat;
     ordered_comm_semiring_linordered_nonzero_semiring =
       ordered_comm_semiring_rat;
     zero_less_one_linordered_nonzero_semiring = zero_less_one_rat}
    : rat linordered_nonzero_semiring);;

let linordered_semidom_rat =
  ({linordered_comm_semiring_strict_linordered_semidom =
      linordered_comm_semiring_strict_rat;
     linordered_nonzero_semiring_linordered_semidom =
       linordered_nonzero_semiring_rat;
     semidom_linordered_semidom = semidom_rat}
    : rat linordered_semidom);;

let ordered_comm_ring_rat =
  ({comm_ring_ordered_comm_ring = comm_ring_rat;
     ordered_cancel_comm_semiring_ordered_comm_ring =
       ordered_cancel_comm_semiring_rat;
     ordered_ring_ordered_comm_ring = ordered_ring_rat}
    : rat ordered_comm_ring);;

let ordered_ring_abs_rat =
  ({ordered_ab_group_add_abs_ordered_ring_abs = ordered_ab_group_add_abs_rat;
     ordered_ring_ordered_ring_abs = ordered_ring_rat}
    : rat ordered_ring_abs);;

let linordered_idom_rat =
  ({ring_char_0_linordered_idom = ring_char_0_rat;
     idom_abs_sgn_linordered_idom = idom_abs_sgn_rat;
     linordered_ring_strict_linordered_idom = linordered_ring_strict_rat;
     linordered_semidom_linordered_idom = linordered_semidom_rat;
     linordered_semiring_1_strict_linordered_idom =
       linordered_semiring_1_strict_rat;
     ordered_comm_ring_linordered_idom = ordered_comm_ring_rat;
     ordered_ring_abs_linordered_idom = ordered_ring_abs_rat}
    : rat linordered_idom);;

type 'a non_strict_order = {ord_non_strict_order : 'a ord};;

type 'a ordered_ab_semigroup =
  {ab_semigroup_add_ordered_ab_semigroup : 'a ab_semigroup_add;
    monoid_add_ordered_ab_semigroup : 'a monoid_add;
    non_strict_order_ordered_ab_semigroup : 'a non_strict_order};;

type 'a ordered_semiring_0a =
  {semiring_0_ordered_semiring_0a : 'a semiring_0;
    ordered_ab_semigroup_ordered_semiring_0 : 'a ordered_ab_semigroup};;

type 'a ordered_semiring_1 =
  {semiring_1_ordered_semiring_1 : 'a semiring_1;
    ordered_semiring_0_ordered_semiring_1 : 'a ordered_semiring_0a};;

type 'a poly_carrier =
  {comm_semiring_1_poly_carrier : 'a comm_semiring_1;
    ordered_semiring_1_poly_carrier : 'a ordered_semiring_1};;

let non_strict_order_rat =
  ({ord_non_strict_order = ord_rat} : rat non_strict_order);;

let ordered_ab_semigroup_rat =
  ({ab_semigroup_add_ordered_ab_semigroup = ab_semigroup_add_rat;
     monoid_add_ordered_ab_semigroup = monoid_add_rat;
     non_strict_order_ordered_ab_semigroup = non_strict_order_rat}
    : rat ordered_ab_semigroup);;

let ordered_semiring_0_rata =
  ({semiring_0_ordered_semiring_0a = semiring_0_rat;
     ordered_ab_semigroup_ordered_semiring_0 = ordered_ab_semigroup_rat}
    : rat ordered_semiring_0a);;

let ordered_semiring_1_rat =
  ({semiring_1_ordered_semiring_1 = semiring_1_rat;
     ordered_semiring_0_ordered_semiring_1 = ordered_semiring_0_rata}
    : rat ordered_semiring_1);;

let poly_carrier_rat =
  ({comm_semiring_1_poly_carrier = comm_semiring_1_rat;
     ordered_semiring_1_poly_carrier = ordered_semiring_1_rat}
    : rat poly_carrier);;

type 'a dense_linorder =
  {dense_order_dense_linorder : 'a dense_order;
    linorder_dense_linorder : 'a linorder};;

type 'a unbounded_dense_linorder =
  {dense_linorder_unbounded_dense_linorder : 'a dense_linorder;
    no_bot_unbounded_dense_linorder : 'a no_bot;
    no_top_unbounded_dense_linorder : 'a no_top};;

type 'a linordered_field =
  {field_abs_sgn_linordered_field : 'a field_abs_sgn;
    field_char_0_linordered_field : 'a field_char_0;
    unbounded_dense_linorder_linordered_field : 'a unbounded_dense_linorder;
    linordered_idom_linordered_field : 'a linordered_idom};;

let dense_linorder_rat =
  ({dense_order_dense_linorder = dense_order_rat;
     linorder_dense_linorder = linorder_rat}
    : rat dense_linorder);;

let unbounded_dense_linorder_rat =
  ({dense_linorder_unbounded_dense_linorder = dense_linorder_rat;
     no_bot_unbounded_dense_linorder = no_bot_rat;
     no_top_unbounded_dense_linorder = no_top_rat}
    : rat unbounded_dense_linorder);;

let linordered_field_rat =
  ({field_abs_sgn_linordered_field = field_abs_sgn_rat;
     field_char_0_linordered_field = field_char_0_rat;
     unbounded_dense_linorder_linordered_field = unbounded_dense_linorder_rat;
     linordered_idom_linordered_field = linordered_idom_rat}
    : rat linordered_field);;

type 'a archimedean_field =
  {linordered_field_archimedean_field : 'a linordered_field};;

type 'a large_ordered_semiring_1 =
  {poly_carrier_large_ordered_semiring_1 : 'a poly_carrier};;

type 'a floor_ceiling =
  {archimedean_field_floor_ceiling : 'a archimedean_field;
    large_ordered_semiring_1_floor_ceiling : 'a large_ordered_semiring_1;
    floor : 'a -> int};;
let floor _A = _A.floor;;

let rec floor_rat p = (let (a, b) = quotient_of p in divide_inta a b);;

let archimedean_field_rat =
  ({linordered_field_archimedean_field = linordered_field_rat} :
    rat archimedean_field);;

let large_ordered_semiring_1_rat =
  ({poly_carrier_large_ordered_semiring_1 = poly_carrier_rat} :
    rat large_ordered_semiring_1);;

let floor_ceiling_rat =
  ({archimedean_field_floor_ceiling = archimedean_field_rat;
     large_ordered_semiring_1_floor_ceiling = large_ordered_semiring_1_rat;
     floor = floor_rat}
    : rat floor_ceiling);;

let rec list_of_dlist _A (Abs_dlist x) = x;;

let rec list_member
  equal x1 y = match equal, x1, y with
    equal, x :: xs, y -> equal x y || list_member equal xs y
    | equal, [], y -> false;;

let rec uminus_set = function Complement b -> b
                     | Collect_set p -> Collect_set (fun x -> not (p x))
                     | a -> Complement a;;

let rec balance
  x0 s t x3 = match x0, s, t, x3 with
    Branch (R, a, w, x, b), s, t, Branch (R, c, y, z, d) ->
      Branch (R, Branch (B, a, w, x, b), s, t, Branch (B, c, y, z, d))
    | Branch (R, Branch (R, a, w, x, b), s, t, c), y, z, Empty ->
        Branch (R, Branch (B, a, w, x, b), s, t, Branch (B, c, y, z, Empty))
    | Branch (R, Branch (R, a, w, x, b), s, t, c), y, z,
        Branch (B, va, vb, vc, vd)
        -> Branch
             (R, Branch (B, a, w, x, b), s, t,
               Branch (B, c, y, z, Branch (B, va, vb, vc, vd)))
    | Branch (R, Empty, w, x, Branch (R, b, s, t, c)), y, z, Empty ->
        Branch (R, Branch (B, Empty, w, x, b), s, t, Branch (B, c, y, z, Empty))
    | Branch (R, Branch (B, va, vb, vc, vd), w, x, Branch (R, b, s, t, c)), y,
        z, Empty
        -> Branch
             (R, Branch (B, Branch (B, va, vb, vc, vd), w, x, b), s, t,
               Branch (B, c, y, z, Empty))
    | Branch (R, Empty, w, x, Branch (R, b, s, t, c)), y, z,
        Branch (B, va, vb, vc, vd)
        -> Branch
             (R, Branch (B, Empty, w, x, b), s, t,
               Branch (B, c, y, z, Branch (B, va, vb, vc, vd)))
    | Branch (R, Branch (B, ve, vf, vg, vh), w, x, Branch (R, b, s, t, c)), y,
        z, Branch (B, va, vb, vc, vd)
        -> Branch
             (R, Branch (B, Branch (B, ve, vf, vg, vh), w, x, b), s, t,
               Branch (B, c, y, z, Branch (B, va, vb, vc, vd)))
    | Empty, w, x, Branch (R, b, s, t, Branch (R, c, y, z, d)) ->
        Branch (R, Branch (B, Empty, w, x, b), s, t, Branch (B, c, y, z, d))
    | Branch (B, va, vb, vc, vd), w, x,
        Branch (R, b, s, t, Branch (R, c, y, z, d))
        -> Branch
             (R, Branch (B, Branch (B, va, vb, vc, vd), w, x, b), s, t,
               Branch (B, c, y, z, d))
    | Empty, w, x, Branch (R, Branch (R, b, s, t, c), y, z, Empty) ->
        Branch (R, Branch (B, Empty, w, x, b), s, t, Branch (B, c, y, z, Empty))
    | Empty, w, x,
        Branch (R, Branch (R, b, s, t, c), y, z, Branch (B, va, vb, vc, vd))
        -> Branch
             (R, Branch (B, Empty, w, x, b), s, t,
               Branch (B, c, y, z, Branch (B, va, vb, vc, vd)))
    | Branch (B, va, vb, vc, vd), w, x,
        Branch (R, Branch (R, b, s, t, c), y, z, Empty)
        -> Branch
             (R, Branch (B, Branch (B, va, vb, vc, vd), w, x, b), s, t,
               Branch (B, c, y, z, Empty))
    | Branch (B, va, vb, vc, vd), w, x,
        Branch (R, Branch (R, b, s, t, c), y, z, Branch (B, ve, vf, vg, vh))
        -> Branch
             (R, Branch (B, Branch (B, va, vb, vc, vd), w, x, b), s, t,
               Branch (B, c, y, z, Branch (B, ve, vf, vg, vh)))
    | Empty, s, t, Empty -> Branch (B, Empty, s, t, Empty)
    | Empty, s, t, Branch (B, va, vb, vc, vd) ->
        Branch (B, Empty, s, t, Branch (B, va, vb, vc, vd))
    | Empty, s, t, Branch (v, Empty, vb, vc, Empty) ->
        Branch (B, Empty, s, t, Branch (v, Empty, vb, vc, Empty))
    | Empty, s, t, Branch (v, Branch (B, ve, vf, vg, vh), vb, vc, Empty) ->
        Branch
          (B, Empty, s, t,
            Branch (v, Branch (B, ve, vf, vg, vh), vb, vc, Empty))
    | Empty, s, t, Branch (v, Empty, vb, vc, Branch (B, vf, vg, vh, vi)) ->
        Branch
          (B, Empty, s, t,
            Branch (v, Empty, vb, vc, Branch (B, vf, vg, vh, vi)))
    | Empty, s, t,
        Branch
          (v, Branch (B, ve, vj, vk, vl), vb, vc, Branch (B, vf, vg, vh, vi))
        -> Branch
             (B, Empty, s, t,
               Branch
                 (v, Branch (B, ve, vj, vk, vl), vb, vc,
                   Branch (B, vf, vg, vh, vi)))
    | Branch (B, va, vb, vc, vd), s, t, Empty ->
        Branch (B, Branch (B, va, vb, vc, vd), s, t, Empty)
    | Branch (B, va, vb, vc, vd), s, t, Branch (B, ve, vf, vg, vh) ->
        Branch (B, Branch (B, va, vb, vc, vd), s, t, Branch (B, ve, vf, vg, vh))
    | Branch (B, va, vb, vc, vd), s, t, Branch (v, Empty, vf, vg, Empty) ->
        Branch
          (B, Branch (B, va, vb, vc, vd), s, t,
            Branch (v, Empty, vf, vg, Empty))
    | Branch (B, va, vb, vc, vd), s, t,
        Branch (v, Branch (B, vi, vj, vk, vl), vf, vg, Empty)
        -> Branch
             (B, Branch (B, va, vb, vc, vd), s, t,
               Branch (v, Branch (B, vi, vj, vk, vl), vf, vg, Empty))
    | Branch (B, va, vb, vc, vd), s, t,
        Branch (v, Empty, vf, vg, Branch (B, vj, vk, vl, vm))
        -> Branch
             (B, Branch (B, va, vb, vc, vd), s, t,
               Branch (v, Empty, vf, vg, Branch (B, vj, vk, vl, vm)))
    | Branch (B, va, vb, vc, vd), s, t,
        Branch
          (v, Branch (B, vi, vn, vo, vp), vf, vg, Branch (B, vj, vk, vl, vm))
        -> Branch
             (B, Branch (B, va, vb, vc, vd), s, t,
               Branch
                 (v, Branch (B, vi, vn, vo, vp), vf, vg,
                   Branch (B, vj, vk, vl, vm)))
    | Branch (v, Empty, vb, vc, Empty), s, t, Empty ->
        Branch (B, Branch (v, Empty, vb, vc, Empty), s, t, Empty)
    | Branch (v, Empty, vb, vc, Branch (B, ve, vf, vg, vh)), s, t, Empty ->
        Branch
          (B, Branch (v, Empty, vb, vc, Branch (B, ve, vf, vg, vh)), s, t,
            Empty)
    | Branch (v, Branch (B, vf, vg, vh, vi), vb, vc, Empty), s, t, Empty ->
        Branch
          (B, Branch (v, Branch (B, vf, vg, vh, vi), vb, vc, Empty), s, t,
            Empty)
    | Branch
        (v, Branch (B, vf, vg, vh, vi), vb, vc, Branch (B, ve, vj, vk, vl)),
        s, t, Empty
        -> Branch
             (B, Branch
                   (v, Branch (B, vf, vg, vh, vi), vb, vc,
                     Branch (B, ve, vj, vk, vl)),
               s, t, Empty)
    | Branch (v, Empty, vf, vg, Empty), s, t, Branch (B, va, vb, vc, vd) ->
        Branch
          (B, Branch (v, Empty, vf, vg, Empty), s, t,
            Branch (B, va, vb, vc, vd))
    | Branch (v, Empty, vf, vg, Branch (B, vi, vj, vk, vl)), s, t,
        Branch (B, va, vb, vc, vd)
        -> Branch
             (B, Branch (v, Empty, vf, vg, Branch (B, vi, vj, vk, vl)), s, t,
               Branch (B, va, vb, vc, vd))
    | Branch (v, Branch (B, vj, vk, vl, vm), vf, vg, Empty), s, t,
        Branch (B, va, vb, vc, vd)
        -> Branch
             (B, Branch (v, Branch (B, vj, vk, vl, vm), vf, vg, Empty), s, t,
               Branch (B, va, vb, vc, vd))
    | Branch
        (v, Branch (B, vj, vk, vl, vm), vf, vg, Branch (B, vi, vn, vo, vp)),
        s, t, Branch (B, va, vb, vc, vd)
        -> Branch
             (B, Branch
                   (v, Branch (B, vj, vk, vl, vm), vf, vg,
                     Branch (B, vi, vn, vo, vp)),
               s, t, Branch (B, va, vb, vc, vd));;

let rec rbt_comp_ins
  c f k v x4 = match c, f, k, v, x4 with
    c, f, k, v, Empty -> Branch (R, Empty, k, v, Empty)
    | c, f, k, v, Branch (B, l, x, y, r) ->
        (match c k x with Eq -> Branch (B, l, x, f k y v, r)
          | Lt -> balance (rbt_comp_ins c f k v l) x y r
          | Gt -> balance l x y (rbt_comp_ins c f k v r))
    | c, f, k, v, Branch (R, l, x, y, r) ->
        (match c k x with Eq -> Branch (R, l, x, f k y v, r)
          | Lt -> Branch (R, rbt_comp_ins c f k v l, x, y, r)
          | Gt -> Branch (R, l, x, y, rbt_comp_ins c f k v r));;

let rec paint c x1 = match c, x1 with c, Empty -> Empty
                | c, Branch (uu, l, k, v, r) -> Branch (c, l, k, v, r);;

let rec rbt_comp_insert_with_key c f k v t = paint B (rbt_comp_ins c f k v t);;

let rec rbt_comp_insert c = rbt_comp_insert_with_key c (fun _ _ nv -> nv);;

let rec impl_of _B (Mapping_RBT x) = x;;

let rec the (Some x2) = x2;;

let rec insertb _A
  xc xd xe =
    Mapping_RBT (rbt_comp_insert (the (ccompare _A)) xc xd (impl_of _A xe));;

let rec rbt_baliR
  t1 ab bb x3 = match t1, ab, bb, x3 with
    t1, ab, bb, Branch (R, t2, aa, ba, Branch (R, t3, a, b, t4)) ->
      Branch (R, Branch (B, t1, ab, bb, t2), aa, ba, Branch (B, t3, a, b, t4))
    | t1, ab, bb, Branch (R, Branch (R, t2, aa, ba, t3), a, b, Empty) ->
        Branch
          (R, Branch (B, t1, ab, bb, t2), aa, ba, Branch (B, t3, a, b, Empty))
    | t1, ab, bb,
        Branch (R, Branch (R, t2, aa, ba, t3), a, b, Branch (B, va, vb, vc, vd))
        -> Branch
             (R, Branch (B, t1, ab, bb, t2), aa, ba,
               Branch (B, t3, a, b, Branch (B, va, vb, vc, vd)))
    | t1, a, b, Empty -> Branch (B, t1, a, b, Empty)
    | t1, a, b, Branch (B, va, vb, vc, vd) ->
        Branch (B, t1, a, b, Branch (B, va, vb, vc, vd))
    | t1, a, b, Branch (v, Empty, vb, vc, Empty) ->
        Branch (B, t1, a, b, Branch (v, Empty, vb, vc, Empty))
    | t1, a, b, Branch (v, Branch (B, ve, vf, vg, vh), vb, vc, Empty) ->
        Branch
          (B, t1, a, b, Branch (v, Branch (B, ve, vf, vg, vh), vb, vc, Empty))
    | t1, a, b, Branch (v, Empty, vb, vc, Branch (B, vf, vg, vh, vi)) ->
        Branch
          (B, t1, a, b, Branch (v, Empty, vb, vc, Branch (B, vf, vg, vh, vi)))
    | t1, a, b,
        Branch
          (v, Branch (B, ve, vj, vk, vl), vb, vc, Branch (B, vf, vg, vh, vi))
        -> Branch
             (B, t1, a, b,
               Branch
                 (v, Branch (B, ve, vj, vk, vl), vb, vc,
                   Branch (B, vf, vg, vh, vi)));;

let rec equal_color x0 x1 = match x0, x1 with R, B -> false
                      | B, R -> false
                      | B, B -> true
                      | R, R -> true;;

let rec suc n = plus_nata n one_nata;;

let rec bheight
  = function Empty -> zero_nata
    | Branch (c, lt, k, v, rt) ->
        (if equal_color c B then suc (bheight lt) else bheight lt);;

let rec rbt_joinR
  l a b r =
    (if less_eq_nat (bheight l) (bheight r) then Branch (R, l, a, b, r)
      else (match l
             with Branch (R, la, ab, ba, ra) ->
               Branch (R, la, ab, ba, rbt_joinR ra a b r)
             | Branch (B, la, ab, ba, ra) ->
               rbt_baliR la ab ba (rbt_joinR ra a b r)));;

let rec rbt_baliL
  x0 a b t4 = match x0, a, b, t4 with
    Branch (R, Branch (R, t1, ab, bb, t2), aa, ba, t3), a, b, t4 ->
      Branch (R, Branch (B, t1, ab, bb, t2), aa, ba, Branch (B, t3, a, b, t4))
    | Branch (R, Empty, ab, bb, Branch (R, t2, aa, ba, t3)), a, b, t4 ->
        Branch
          (R, Branch (B, Empty, ab, bb, t2), aa, ba, Branch (B, t3, a, b, t4))
    | Branch
        (R, Branch (B, va, vb, vc, vd), ab, bb, Branch (R, t2, aa, ba, t3)),
        a, b, t4
        -> Branch
             (R, Branch (B, Branch (B, va, vb, vc, vd), ab, bb, t2), aa, ba,
               Branch (B, t3, a, b, t4))
    | Empty, a, b, t2 -> Branch (B, Empty, a, b, t2)
    | Branch (B, va, vb, vc, vd), a, b, t2 ->
        Branch (B, Branch (B, va, vb, vc, vd), a, b, t2)
    | Branch (v, Empty, vb, vc, Empty), a, b, t2 ->
        Branch (B, Branch (v, Empty, vb, vc, Empty), a, b, t2)
    | Branch (v, Empty, vb, vc, Branch (B, ve, vf, vg, vh)), a, b, t2 ->
        Branch
          (B, Branch (v, Empty, vb, vc, Branch (B, ve, vf, vg, vh)), a, b, t2)
    | Branch (v, Branch (B, vf, vg, vh, vi), vb, vc, Empty), a, b, t2 ->
        Branch
          (B, Branch (v, Branch (B, vf, vg, vh, vi), vb, vc, Empty), a, b, t2)
    | Branch
        (v, Branch (B, vf, vg, vh, vi), vb, vc, Branch (B, ve, vj, vk, vl)),
        a, b, t2
        -> Branch
             (B, Branch
                   (v, Branch (B, vf, vg, vh, vi), vb, vc,
                     Branch (B, ve, vj, vk, vl)),
               a, b, t2);;

let rec rbt_joinL
  l a b r =
    (if less_eq_nat (bheight r) (bheight l) then Branch (R, l, a, b, r)
      else (match r
             with Branch (R, la, ab, ba, ra) ->
               Branch (R, rbt_joinL l a b la, ab, ba, ra)
             | Branch (B, la, ab, ba, ra) ->
               rbt_baliL (rbt_joinL l a b la) ab ba ra));;

let rec rbt_join
  l a b r =
    (let bhl = bheight l in
     let bhr = bheight r in
      (if less_nat bhr bhl then paint B (rbt_joinR l a b r)
        else (if less_nat bhl bhr then paint B (rbt_joinL l a b r)
               else Branch (B, l, a, b, r))));;

let rec rbt_split_comp
  c x1 k = match c, x1, k with c, Empty, k -> (Empty, (None, Empty))
    | c, Branch (uu, l, a, b, r), x ->
        (match c x a with Eq -> (l, (Some b, r))
          | Lt ->
            (let (l1, (beta, l2)) = rbt_split_comp c l x in
              (l1, (beta, rbt_join l2 a b r)))
          | Gt ->
            (let (r1, (beta, r2)) = rbt_split_comp c r x in
              (rbt_join l a b r1, (beta, r2))));;

let rec folda
  f xa1 x = match f, xa1, x with
    f, Branch (c, lt, k, v, rt), x -> folda f rt (f k v (folda f lt x))
    | f, Empty, x -> x;;

let rec rbt_comp_union_swap_rec
  c f gamma t1 t2 =
    (let bh1 = bheight t1 in
     let bh2 = bheight t2 in
     let (gammaa, (t2a, (bh2a, (t1a, _)))) =
       (if less_nat bh1 bh2 then (not gamma, (t1, (bh1, (t2, bh2))))
         else (gamma, (t2, (bh2, (t1, bh1)))))
       in
     let fa = (if gammaa then (fun k v va -> f k va v) else f) in
      (if less_nat bh2a (nat_of_integer (Z.of_int 4))
        then folda (rbt_comp_insert_with_key c fa) t2a t1a
        else (match t1a with Empty -> t2a
               | Branch (_, l1, a, b, r1) ->
                 (let (l2, (beta, r2)) = rbt_split_comp c t2a a in
                   rbt_join (rbt_comp_union_swap_rec c f gammaa l1 l2) a
                     (match beta with None -> b | Some ca -> fa a b ca)
                     (rbt_comp_union_swap_rec c f gammaa r1 r2)))));;

let rec rbt_comp_union_with_key
  c f t1 t2 = paint B (rbt_comp_union_swap_rec c f false t1 t2);;

let rec join _A
  xc xd xe =
    Mapping_RBT
      (rbt_comp_union_with_key (the (ccompare _A)) xc (impl_of _A xd)
        (impl_of _A xe));;

let rec list_insert
  equal x xs = (if list_member equal xs x then xs else x :: xs);;

let rec inserta _A
  xb xc = Abs_dlist (list_insert (the (ceq _A)) xb (list_of_dlist _A xc));;

let rec fold f x1 s = match f, x1, s with f, x :: xs, s -> fold f xs (f x s)
               | f, [], s -> s;;

let rec foldc _A x xc = fold x (list_of_dlist _A xc);;

let rec union _A = foldc _A (inserta _A);;

let rec memberc _A xa = list_member (the (ceq _A)) (list_of_dlist _A xa);;

let rec equal_option _A x0 x1 = match x0, x1 with None, Some x2 -> false
                          | Some x2, None -> false
                          | Some x2, Some y2 -> eq _A x2 y2
                          | None, None -> true;;

let rec rbt_comp_lookup
  c x1 k = match c, x1, k with c, Empty, k -> None
    | c, Branch (uu, l, x, y, r), k ->
        (match c k x with Eq -> Some y | Lt -> rbt_comp_lookup c l k
          | Gt -> rbt_comp_lookup c r k);;

let rec lookup _A xa = rbt_comp_lookup (the (ccompare _A)) (impl_of _A xa);;

let rec equal_unita u v = true;;

let equal_unit = ({equal = equal_unita} : unit equal);;

let rec memberb _A t x = equal_option equal_unit (lookup _A t x) (Some ());;

let rec member (_A1, _A2)
  x xa1 = match x, xa1 with
    x, Set_Monad xs ->
      (match ceq _A1
        with None ->
          failwith "member Set_Monad: ceq = None"
            (fun _ -> member (_A1, _A2) x (Set_Monad xs))
        | Some eq -> list_member eq xs x)
    | xa, Complement x -> not (member (_A1, _A2) xa x)
    | x, RBT_set rbt -> memberb _A2 rbt x
    | x, DList_set dxs -> memberc _A1 dxs x
    | x, Collect_set a -> a x;;

let rec id x = (fun xa -> xa) x;;

let rec is_none = function Some x -> false
                  | None -> true;;

let rec filtera
  p x1 = match p, x1 with p, [] -> []
    | p, x :: xs -> (if p x then x :: filtera p xs else filtera p xs);;

let rec inter_list _A
  xb xc =
    Mapping_RBT
      (fold (fun k -> rbt_comp_insert (the (ccompare _A)) k ())
        (filtera
          (fun x ->
            not (is_none
                  (rbt_comp_lookup (the (ccompare _A)) (impl_of _A xb) x)))
          xc)
        Empty);;

let rec gen_length n x1 = match n, x1 with n, x :: xs -> gen_length (suc n) xs
                     | n, [] -> n;;

let rec size_list x = gen_length zero_nata x;;

let rec map_prod f g (a, b) = (f a, g b);;

let rec divmod_nat
  m n = (let k = integer_of_nat m in
         let l = integer_of_nat n in
          map_prod nat_of_integer nat_of_integer
            (if Z.equal k Z.zero then (Z.zero, Z.zero)
              else (if Z.equal l Z.zero then (Z.zero, k) else Z.div_rem k l)));;

let rec apfst f (x, y) = (f x, y);;

let rec rbtreeify_g
  n kvs =
    (if equal_nata n zero_nata || equal_nata n one_nata then (Empty, kvs)
      else (let (na, r) = divmod_nat n (nat_of_integer (Z.of_int 2)) in
             (if equal_nata r zero_nata
               then (let (t1, (k, v) :: kvsa) = rbtreeify_g na kvs in
                      apfst (fun a -> Branch (B, t1, k, v, a))
                        (rbtreeify_g na kvsa))
               else (let (t1, (k, v) :: kvsa) = rbtreeify_f na kvs in
                      apfst (fun a -> Branch (B, t1, k, v, a))
                        (rbtreeify_g na kvsa)))))
and rbtreeify_f
  n kvs =
    (if equal_nata n zero_nata then (Empty, kvs)
      else (if equal_nata n one_nata
             then (let (k, v) :: kvsa = kvs in
                    (Branch (R, Empty, k, v, Empty), kvsa))
             else (let (na, r) = divmod_nat n (nat_of_integer (Z.of_int 2)) in
                    (if equal_nata r zero_nata
                      then (let (t1, (k, v) :: kvsa) = rbtreeify_f na kvs in
                             apfst (fun a -> Branch (B, t1, k, v, a))
                               (rbtreeify_g na kvsa))
                      else (let (t1, (k, v) :: kvsa) = rbtreeify_f na kvs in
                             apfst (fun a -> Branch (B, t1, k, v, a))
                               (rbtreeify_f na kvsa))))));;

let rec rbtreeify kvs = fst (rbtreeify_g (suc (size_list kvs)) kvs);;

let rec gen_entries
  kvts x1 = match kvts, x1 with
    kvts, Branch (c, l, k, v, r) -> gen_entries (((k, v), r) :: kvts) l
    | (kv, t) :: kvts, Empty -> kv :: gen_entries kvts t
    | [], Empty -> [];;

let rec entries x = gen_entries [] x;;

let rec filterc _A
  xb xc = Mapping_RBT (rbtreeify (filtera xb (entries (impl_of _A xc))));;

let rec map_filter
  f x1 = match f, x1 with f, [] -> []
    | f, x :: xs ->
        (match f x with None -> map_filter f xs
          | Some y -> y :: map_filter f xs);;

let rec map_filter_comp_inter
  c f t1 t2 =
    map_filter
      (fun (k, v) ->
        (match rbt_comp_lookup c t1 k with None -> None
          | Some va -> Some (k, f k va v)))
      (entries t2);;

let rec is_rbt_empty
  t = (match t with Empty -> true | Branch (_, _, _, _, _) -> false);;

let rec rbt_split_min
  = function Empty -> failwith "undefined"
    | Branch (uu, l, a, b, r) ->
        (if is_rbt_empty l then (a, (b, r))
          else (let (aa, (ba, la)) = rbt_split_min l in
                 (aa, (ba, rbt_join la a b r))));;

let rec rbt_join2
  l r = (if is_rbt_empty r then l
          else (let (a, (b, c)) = rbt_split_min r in rbt_join l a b c));;

let rec rbt_comp_inter_swap_rec
  c f gamma t1 t2 =
    (let bh1 = bheight t1 in
     let bh2 = bheight t2 in
     let (gammaa, (t2a, (bh2a, (t1a, _)))) =
       (if less_nat bh1 bh2 then (not gamma, (t1, (bh1, (t2, bh2))))
         else (gamma, (t2, (bh2, (t1, bh1)))))
       in
     let fa = (if gammaa then (fun k v va -> f k va v) else f) in
      (if less_nat bh2a (nat_of_integer (Z.of_int 4))
        then rbtreeify (map_filter_comp_inter c fa t1a t2a)
        else (match t1a with Empty -> Empty
               | Branch (_, l1, a, b, r1) ->
                 (let (l2, (beta, r2)) = rbt_split_comp c t2a a in
                  let l = rbt_comp_inter_swap_rec c f gammaa l1 l2 in
                  let r = rbt_comp_inter_swap_rec c f gammaa r1 r2 in
                   (match beta with None -> rbt_join2 l r
                     | Some ba -> rbt_join l a (fa a b ba) r)))));;

let rec rbt_comp_inter_with_key
  c f t1 t2 = paint B (rbt_comp_inter_swap_rec c f false t1 t2);;

let rec meet _A
  xc xd xe =
    Mapping_RBT
      (rbt_comp_inter_with_key (the (ccompare _A)) xc (impl_of _A xd)
        (impl_of _A xe));;

let rec filterb _A xb xc = Abs_dlist (filtera xb (list_of_dlist _A xc));;

let rec inf_seta (_A1, _A2)
  g ga = match g, ga with
    RBT_set rbt1, Set_Monad xs ->
      (match ccompare _A2
        with None ->
          failwith "inter RBT_set Set_Monad: ccompare = None"
            (fun _ -> inf_seta (_A1, _A2) (RBT_set rbt1) (Set_Monad xs))
        | Some _ -> RBT_set (inter_list _A2 rbt1 xs))
    | RBT_set rbt, DList_set dxs ->
        (match ccompare _A2
          with None ->
            failwith "inter RBT_set DList_set: ccompare = None"
              (fun _ -> inf_seta (_A1, _A2) (RBT_set rbt) (DList_set dxs))
          | Some _ ->
            (match ceq _A1
              with None ->
                failwith "inter RBT_set DList_set: ceq = None"
                  (fun _ -> inf_seta (_A1, _A2) (RBT_set rbt) (DList_set dxs))
              | Some _ -> RBT_set (inter_list _A2 rbt (list_of_dlist _A1 dxs))))
    | RBT_set rbt1, RBT_set rbt2 ->
        (match ccompare _A2
          with None ->
            failwith "inter RBT_set RBT_set: ccompare = None"
              (fun _ -> inf_seta (_A1, _A2) (RBT_set rbt1) (RBT_set rbt2))
          | Some _ -> RBT_set (meet _A2 (fun _ _ -> id) rbt1 rbt2))
    | DList_set dxs1, Set_Monad xs ->
        (match ceq _A1
          with None ->
            failwith "inter DList_set Set_Monad: ceq = None"
              (fun _ -> inf_seta (_A1, _A2) (DList_set dxs1) (Set_Monad xs))
          | Some eq -> DList_set (filterb _A1 (list_member eq xs) dxs1))
    | DList_set dxs1, DList_set dxs2 ->
        (match ceq _A1
          with None ->
            failwith "inter DList_set DList_set: ceq = None"
              (fun _ -> inf_seta (_A1, _A2) (DList_set dxs1) (DList_set dxs2))
          | Some _ -> DList_set (filterb _A1 (memberc _A1 dxs2) dxs1))
    | DList_set dxs, RBT_set rbt ->
        (match ccompare _A2
          with None ->
            failwith "inter DList_set RBT_set: ccompare = None"
              (fun _ -> inf_seta (_A1, _A2) (DList_set dxs) (RBT_set rbt))
          | Some _ ->
            (match ceq _A1
              with None ->
                failwith "inter DList_set RBT_set: ceq = None"
                  (fun _ -> inf_seta (_A1, _A2) (DList_set dxs) (RBT_set rbt))
              | Some _ -> RBT_set (inter_list _A2 rbt (list_of_dlist _A1 dxs))))
    | Set_Monad xs1, Set_Monad xs2 ->
        (match ceq _A1
          with None ->
            failwith "inter Set_Monad Set_Monad: ceq = None"
              (fun _ -> inf_seta (_A1, _A2) (Set_Monad xs1) (Set_Monad xs2))
          | Some eq -> Set_Monad (filtera (list_member eq xs2) xs1))
    | Set_Monad xs, DList_set dxs2 ->
        (match ceq _A1
          with None ->
            failwith "inter Set_Monad DList_set: ceq = None"
              (fun _ -> inf_seta (_A1, _A2) (Set_Monad xs) (DList_set dxs2))
          | Some eq -> DList_set (filterb _A1 (list_member eq xs) dxs2))
    | Set_Monad xs, RBT_set rbt1 ->
        (match ccompare _A2
          with None ->
            failwith "inter Set_Monad RBT_set: ccompare = None"
              (fun _ -> inf_seta (_A1, _A2) (RBT_set rbt1) (Set_Monad xs))
          | Some _ -> RBT_set (inter_list _A2 rbt1 xs))
    | Complement ba, Complement b -> Complement (sup_seta (_A1, _A2) ba b)
    | g, RBT_set rbt2 ->
        (match ccompare _A2
          with None ->
            failwith "inter RBT_set2: ccompare = None"
              (fun _ -> inf_seta (_A1, _A2) g (RBT_set rbt2))
          | Some _ ->
            RBT_set
              (filterc _A2 (comp (fun x -> member (_A1, _A2) x g) fst) rbt2))
    | RBT_set rbt1, g ->
        (match ccompare _A2
          with None ->
            failwith "inter RBT_set1: ccompare = None"
              (fun _ -> inf_seta (_A1, _A2) (RBT_set rbt1) g)
          | Some _ ->
            RBT_set
              (filterc _A2 (comp (fun x -> member (_A1, _A2) x g) fst) rbt1))
    | h, DList_set dxs2 ->
        (match ceq _A1
          with None ->
            failwith "inter DList_set2: ceq = None"
              (fun _ -> inf_seta (_A1, _A2) h (DList_set dxs2))
          | Some _ ->
            DList_set (filterb _A1 (fun x -> member (_A1, _A2) x h) dxs2))
    | DList_set dxs1, h ->
        (match ceq _A1
          with None ->
            failwith "inter DList_set1: ceq = None"
              (fun _ -> inf_seta (_A1, _A2) (DList_set dxs1) h)
          | Some _ ->
            DList_set (filterb _A1 (fun x -> member (_A1, _A2) x h) dxs1))
    | i, Set_Monad xs -> Set_Monad (filtera (fun x -> member (_A1, _A2) x i) xs)
    | Set_Monad xs, i -> Set_Monad (filtera (fun x -> member (_A1, _A2) x i) xs)
    | j, Collect_set a -> Collect_set (fun x -> a x && member (_A1, _A2) x j)
    | Collect_set a, j -> Collect_set (fun x -> a x && member (_A1, _A2) x j)
and sup_seta (_A1, _A2)
  ba b = match ba, b with
    ba, Complement b -> Complement (inf_seta (_A1, _A2) (uminus_set ba) b)
    | Complement ba, b -> Complement (inf_seta (_A1, _A2) ba (uminus_set b))
    | b, Collect_set a -> Collect_set (fun x -> a x || member (_A1, _A2) x b)
    | Collect_set a, b -> Collect_set (fun x -> a x || member (_A1, _A2) x b)
    | Set_Monad xs, Set_Monad ys -> Set_Monad (xs @ ys)
    | DList_set dxs1, Set_Monad ws ->
        (match ceq _A1
          with None ->
            failwith "union DList_set Set_Monad: ceq = None"
              (fun _ -> sup_seta (_A1, _A2) (DList_set dxs1) (Set_Monad ws))
          | Some _ -> DList_set (fold (inserta _A1) ws dxs1))
    | Set_Monad ws, DList_set dxs2 ->
        (match ceq _A1
          with None ->
            failwith "union Set_Monad DList_set: ceq = None"
              (fun _ -> sup_seta (_A1, _A2) (Set_Monad ws) (DList_set dxs2))
          | Some _ -> DList_set (fold (inserta _A1) ws dxs2))
    | RBT_set rbt1, Set_Monad zs ->
        (match ccompare _A2
          with None ->
            failwith "union RBT_set Set_Monad: ccompare = None"
              (fun _ -> sup_seta (_A1, _A2) (RBT_set rbt1) (Set_Monad zs))
          | Some _ -> RBT_set (fold (fun k -> insertb _A2 k ()) zs rbt1))
    | Set_Monad zs, RBT_set rbt2 ->
        (match ccompare _A2
          with None ->
            failwith "union Set_Monad RBT_set: ccompare = None"
              (fun _ -> sup_seta (_A1, _A2) (Set_Monad zs) (RBT_set rbt2))
          | Some _ -> RBT_set (fold (fun k -> insertb _A2 k ()) zs rbt2))
    | DList_set dxs1, DList_set dxs2 ->
        (match ceq _A1
          with None ->
            failwith "union DList_set DList_set: ceq = None"
              (fun _ -> sup_seta (_A1, _A2) (DList_set dxs1) (DList_set dxs2))
          | Some _ -> DList_set (union _A1 dxs1 dxs2))
    | DList_set dxs, RBT_set rbt ->
        (match ccompare _A2
          with None ->
            failwith "union DList_set RBT_set: ccompare = None"
              (fun _ -> sup_seta (_A1, _A2) (RBT_set rbt) (DList_set dxs))
          | Some _ ->
            (match ceq _A1
              with None ->
                failwith "union DList_set RBT_set: ceq = None"
                  (fun _ -> sup_seta (_A1, _A2) (RBT_set rbt) (DList_set dxs))
              | Some _ ->
                RBT_set (foldc _A1 (fun k -> insertb _A2 k ()) dxs rbt)))
    | RBT_set rbt, DList_set dxs ->
        (match ccompare _A2
          with None ->
            failwith "union RBT_set DList_set: ccompare = None"
              (fun _ -> sup_seta (_A1, _A2) (RBT_set rbt) (DList_set dxs))
          | Some _ ->
            (match ceq _A1
              with None ->
                failwith "union RBT_set DList_set: ceq = None"
                  (fun _ -> sup_seta (_A1, _A2) (RBT_set rbt) (DList_set dxs))
              | Some _ ->
                RBT_set (foldc _A1 (fun k -> insertb _A2 k ()) dxs rbt)))
    | RBT_set rbt1, RBT_set rbt2 ->
        (match ccompare _A2
          with None ->
            failwith "union RBT_set RBT_set: ccompare = None"
              (fun _ -> sup_seta (_A1, _A2) (RBT_set rbt1) (RBT_set rbt2))
          | Some _ -> RBT_set (join _A2 (fun _ _ -> id) rbt1 rbt2));;

let rec inf_set (_A1, _A2) = ({inf = inf_seta (_A1, _A2)} : 'a set inf);;

let rec sup_set (_A1, _A2) = ({sup = sup_seta (_A1, _A2)} : 'a set sup);;

let rec equal_order x0 x1 = match x0, x1 with Lt, Gt -> false
                      | Gt, Lt -> false
                      | Eq, Gt -> false
                      | Gt, Eq -> false
                      | Eq, Lt -> false
                      | Lt, Eq -> false
                      | Gt, Gt -> true
                      | Lt, Lt -> true
                      | Eq, Eq -> true;;

type ('a, 'b) generator = Generator of (('b -> bool) * ('b -> 'a * 'b));;

let rec generator (Generator x) = x;;

let rec has_next g = fst (generator g);;

let rec next g = snd (generator g);;

let rec sorted_list_subset_fusion
  less eq g1 g2 s1 s2 =
    (if has_next g1 s1
      then (let (x, s1a) = next g1 s1 in
             has_next g2 s2 &&
               (let (y, s2a) = next g2 s2 in
                 (if eq x y then sorted_list_subset_fusion less eq g1 g2 s1a s2a
                   else less y x &&
                          sorted_list_subset_fusion less eq g1 g2 s1 s2a)))
      else true);;

let rec list_all_fusion
  g p s =
    (if has_next g s
      then (let (x, sa) = next g s in p x && list_all_fusion g p sa)
      else true);;

let rec rbt_keys_next
  = function ((k, t) :: kts, Empty) -> (k, (kts, t))
    | (kts, Branch (c, l, k, v, r)) -> rbt_keys_next ((k, r) :: kts, l);;

let rec rbt_has_next = function ([], Empty) -> false
                       | (vb :: vc, va) -> true
                       | (v, Branch (vb, vc, vd, ve, vf)) -> true;;

let rbt_keys_generator :
  ('a, (('a * ('a, 'b) rbt) list * ('a, 'b) rbt)) generator
  = Generator (rbt_has_next, rbt_keys_next);;

let rec lt_of_comp
  acomp x y = (match acomp x y with Eq -> false | Lt -> true | Gt -> false);;

let rec list_all p x1 = match p, x1 with p, [] -> true
                   | p, x :: xs -> p x && list_all p xs;;

let rec dlist_all _A x xc = list_all x (list_of_dlist _A xc);;

let rec rbt_init x = ([], x);;

let rec init _A xa = rbt_init (impl_of _A xa);;

let rec collect _A
  p = (match cEnum _A with None -> Collect_set p
        | Some (enum, _) -> Set_Monad (filtera p enum));;

let rec less_eq_set (_A1, _A2, _A3)
  x0 c = match x0, c with
    RBT_set rbt1, RBT_set rbt2 ->
      (match ccompare _A3
        with None ->
          failwith "subset RBT_set RBT_set: ccompare = None"
            (fun _ -> less_eq_set (_A1, _A2, _A3) (RBT_set rbt1) (RBT_set rbt2))
        | Some c ->
          (match ceq _A2
            with None ->
              sorted_list_subset_fusion (lt_of_comp c)
                (fun x y -> equal_order (c x y) Eq) rbt_keys_generator
                rbt_keys_generator (init _A3 rbt1) (init _A3 rbt2)
            | Some eq ->
              sorted_list_subset_fusion (lt_of_comp c) eq rbt_keys_generator
                rbt_keys_generator (init _A3 rbt1) (init _A3 rbt2)))
    | Complement a1, Complement a2 -> less_eq_set (_A1, _A2, _A3) a2 a1
    | Collect_set p, Complement a ->
        less_eq_set (_A1, _A2, _A3) a (collect _A1 (fun x -> not (p x)))
    | Set_Monad xs, c -> list_all (fun x -> member (_A2, _A3) x c) xs
    | DList_set dxs, c ->
        (match ceq _A2
          with None ->
            failwith "subset DList_set1: ceq = None"
              (fun _ -> less_eq_set (_A1, _A2, _A3) (DList_set dxs) c)
          | Some _ -> dlist_all _A2 (fun x -> member (_A2, _A3) x c) dxs)
    | RBT_set rbt, b ->
        (match ccompare _A3
          with None ->
            failwith "subset RBT_set1: ccompare = None"
              (fun _ -> less_eq_set (_A1, _A2, _A3) (RBT_set rbt) b)
          | Some _ ->
            list_all_fusion rbt_keys_generator (fun x -> member (_A2, _A3) x b)
              (init _A3 rbt));;

let rec less_set (_A1, _A2, _A3)
  a b = less_eq_set (_A1, _A2, _A3) a b &&
          not (less_eq_set (_A1, _A2, _A3) b a);;

let rec ord_set (_A1, _A2, _A3) =
  ({less_eq = less_eq_set (_A1, _A2, _A3); less = less_set (_A1, _A2, _A3)} :
    'a set ord);;

let rec preorder_set (_A1, _A2, _A3) =
  ({ord_preorder = (ord_set (_A1, _A2, _A3))} : 'a set preorder);;

let rec order_set (_A1, _A2, _A3) =
  ({preorder_order = (preorder_set (_A1, _A2, _A3))} : 'a set order);;

let rec semilattice_sup_set (_A1, _A2, _A3) =
  ({sup_semilattice_sup = (sup_set (_A2, _A3));
     order_semilattice_sup = (order_set (_A1, _A2, _A3))}
    : 'a set semilattice_sup);;

let rec semilattice_inf_set (_A1, _A2, _A3) =
  ({inf_semilattice_inf = (inf_set (_A2, _A3));
     order_semilattice_inf = (order_set (_A1, _A2, _A3))}
    : 'a set semilattice_inf);;

let rec lattice_set (_A1, _A2, _A3) =
  ({semilattice_inf_lattice = (semilattice_inf_set (_A1, _A2, _A3));
     semilattice_sup_lattice = (semilattice_sup_set (_A1, _A2, _A3))}
    : 'a set lattice);;

let rec list_all2_fusion
  p g1 g2 s1 s2 =
    (if has_next g1 s1
      then has_next g2 s2 &&
             (let (x, s1a) = next g1 s1 in
              let (y, s2a) = next g2 s2 in
               p x y && list_all2_fusion p g1 g2 s1a s2a)
      else not (has_next g2 s2));;

let rec set_eq (_A1, _A2, _A3)
  a b = match a, b with
    RBT_set rbt1, RBT_set rbt2 ->
      (match ccompare _A3
        with None ->
          failwith "set_eq RBT_set RBT_set: ccompare = None"
            (fun _ -> set_eq (_A1, _A2, _A3) (RBT_set rbt1) (RBT_set rbt2))
        | Some c ->
          (match ceq _A2
            with None ->
              list_all2_fusion (fun x y -> equal_order (c x y) Eq)
                rbt_keys_generator rbt_keys_generator (init _A3 rbt1)
                (init _A3 rbt2)
            | Some eq ->
              list_all2_fusion eq rbt_keys_generator rbt_keys_generator
                (init _A3 rbt1) (init _A3 rbt2)))
    | Complement a, Complement b -> set_eq (_A1, _A2, _A3) a b
    | a, b ->
        less_eq_set (_A1, _A2, _A3) a b && less_eq_set (_A1, _A2, _A3) b a;;

let rec ceq_seta (_A1, _A2, _A3)
  = (match ceq _A2 with None -> None
      | Some _ -> Some (set_eq (_A1, _A2, _A3)));;

let rec ceq_set (_A1, _A2, _A3) =
  ({ceq = ceq_seta (_A1, _A2, _A3)} : 'a set ceq);;

let set_impl_seta : ('a set, set_impla) phantom = Phantom Set_Choose;;

let set_impl_set = ({set_impl = set_impl_seta} : 'a set set_impl);;

let rec of_phantom (Phantom x) = x;;

let rec finite_UNIV_seta _A = Phantom (of_phantom (finite_UNIV _A));;

let rec finite_UNIV_set _A =
  ({finite_UNIV = finite_UNIV_seta _A} : 'a set finite_UNIV);;

let rec set_less_eq_aux_Compl_fusion
  less proper_interval g1 g2 ao s1 s2 =
    (if has_next g1 s1
      then (if has_next g2 s2
             then (let (x, s1a) = next g1 s1 in
                   let (y, s2a) = next g2 s2 in
                    (if less x y
                      then proper_interval ao (Some x) ||
                             set_less_eq_aux_Compl_fusion less proper_interval
                               g1 g2 (Some x) s1a s2
                      else (if less y x
                             then proper_interval ao (Some y) ||
                                    set_less_eq_aux_Compl_fusion less
                                      proper_interval g1 g2 (Some y) s1 s2a
                             else proper_interval ao (Some y))))
             else true)
      else true);;

let rec compl_set_less_eq_aux_fusion
  less proper_interval g1 g2 ao s1 s2 =
    (if has_next g1 s1
      then (let (x, s1a) = next g1 s1 in
             (if has_next g2 s2
               then (let (y, s2a) = next g2 s2 in
                      (if less x y
                        then not (proper_interval ao (Some x)) &&
                               compl_set_less_eq_aux_fusion less proper_interval
                                 g1 g2 (Some x) s1a s2
                        else (if less y x
                               then not (proper_interval ao (Some y)) &&
                                      compl_set_less_eq_aux_fusion less
proper_interval g1 g2 (Some y) s1 s2a
                               else not (proper_interval ao (Some y)))))
               else not (proper_interval ao (Some x)) &&
                      compl_set_less_eq_aux_fusion less proper_interval g1 g2
                        (Some x) s1a s2))
      else (if has_next g2 s2
             then (let (y, s2a) = next g2 s2 in
                    not (proper_interval ao (Some y)) &&
                      compl_set_less_eq_aux_fusion less proper_interval g1 g2
                        (Some y) s1 s2a)
             else not (proper_interval ao None)));;

let rec set_less_eq_aux_Compl
  less proper_interval ao xs ys = match less, proper_interval, ao, xs, ys with
    less, proper_interval, ao, x :: xs, y :: ys ->
      (if less x y
        then proper_interval ao (Some x) ||
               set_less_eq_aux_Compl less proper_interval (Some x) xs (y :: ys)
        else (if less y x
               then proper_interval ao (Some y) ||
                      set_less_eq_aux_Compl less proper_interval (Some y)
                        (x :: xs) ys
               else proper_interval ao (Some y)))
    | less, proper_interval, ao, xs, [] -> true
    | less, proper_interval, ao, [], ys -> true;;

let rec compl_set_less_eq_aux
  less proper_interval ao x3 x4 = match less, proper_interval, ao, x3, x4 with
    less, proper_interval, ao, x :: xs, y :: ys ->
      (if less x y
        then not (proper_interval ao (Some x)) &&
               compl_set_less_eq_aux less proper_interval (Some x) xs (y :: ys)
        else (if less y x
               then not (proper_interval ao (Some y)) &&
                      compl_set_less_eq_aux less proper_interval (Some y)
                        (x :: xs) ys
               else not (proper_interval ao (Some y))))
    | less, proper_interval, ao, x :: xs, [] ->
        not (proper_interval ao (Some x)) &&
          compl_set_less_eq_aux less proper_interval (Some x) xs []
    | less, proper_interval, ao, [], y :: ys ->
        not (proper_interval ao (Some y)) &&
          compl_set_less_eq_aux less proper_interval (Some y) [] ys
    | less, proper_interval, ao, [], [] -> not (proper_interval ao None);;

let rec lexord_eq_fusion
  less g1 g2 s1 s2 =
    (if has_next g1 s1
      then has_next g2 s2 &&
             (let (x, s1a) = next g1 s1 in
              let (y, s2a) = next g2 s2 in
               less x y ||
                 not (less y x) && lexord_eq_fusion less g1 g2 s1a s2a)
      else true);;

let rec remdups_sorted
  less x1 = match less, x1 with
    less, x :: y :: xs ->
      (if less x y then x :: remdups_sorted less (y :: xs)
        else remdups_sorted less (y :: xs))
    | less, [x] -> [x]
    | less, [] -> [];;

let rec quicksort_acc
  less ac x2 = match less, ac, x2 with
    less, ac, x :: v :: va -> quicksort_part less ac x [] [] [] (v :: va)
    | less, ac, [x] -> x :: ac
    | less, ac, [] -> ac
and quicksort_part
  less ac x lts eqs gts xa6 = match less, ac, x, lts, eqs, gts, xa6 with
    less, ac, x, lts, eqs, gts, z :: zs ->
      (if less x z then quicksort_part less ac x lts eqs (z :: gts) zs
        else (if less z x then quicksort_part less ac x (z :: lts) eqs gts zs
               else quicksort_part less ac x lts (z :: eqs) gts zs))
    | less, ac, x, lts, eqs, gts, [] ->
        quicksort_acc less (eqs @ x :: quicksort_acc less ac gts) lts;;

let rec quicksort less = quicksort_acc less [];;

let rec gen_keys
  kts x1 = match kts, x1 with
    kts, Branch (c, l, k, v, r) -> gen_keys ((k, r) :: kts) l
    | (k, t) :: kts, Empty -> k :: gen_keys kts t
    | [], Empty -> [];;

let rec keys x = gen_keys [] x;;

let rec keysa _A xa = keys (impl_of _A xa);;

let rec csorted_list_of_set (_A1, _A2)
  = function
    Set_Monad xs ->
      (match ccompare _A2
        with None ->
          failwith "csorted_list_of_set Set_Monad: ccompare = None"
            (fun _ -> csorted_list_of_set (_A1, _A2) (Set_Monad xs))
        | Some c -> remdups_sorted (lt_of_comp c) (quicksort (lt_of_comp c) xs))
    | DList_set dxs ->
        (match ceq _A1
          with None ->
            failwith "csorted_list_of_set DList_set: ceq = None"
              (fun _ -> csorted_list_of_set (_A1, _A2) (DList_set dxs))
          | Some _ ->
            (match ccompare _A2
              with None ->
                failwith "csorted_list_of_set DList_set: ccompare = None"
                  (fun _ -> csorted_list_of_set (_A1, _A2) (DList_set dxs))
              | Some c -> quicksort (lt_of_comp c) (list_of_dlist _A1 dxs)))
    | RBT_set rbt ->
        (match ccompare _A2
          with None ->
            failwith "csorted_list_of_set RBT_set: ccompare = None"
              (fun _ -> csorted_list_of_set (_A1, _A2) (RBT_set rbt))
          | Some _ -> keysa _A2 rbt);;

let rec emptya _A = Mapping_RBT Empty;;

let rec empty _A = Abs_dlist [];;

let rec set_empty_choose (_A1, _A2)
  = (match ccompare _A2
      with None ->
        (match ceq _A1 with None -> Set_Monad []
          | Some _ -> DList_set (empty _A1))
      | Some _ -> RBT_set (emptya _A2));;

let rec set_empty (_A1, _A2)
  = function Set_Choose -> set_empty_choose (_A1, _A2)
    | Set_Monada -> Set_Monad []
    | Set_RBT -> RBT_set (emptya _A2)
    | Set_DList -> DList_set (empty _A1)
    | Set_Collect -> Collect_set (fun _ -> false);;

let rec bot_set (_A1, _A2, _A3)
  = set_empty (_A1, _A2) (of_phantom (set_impl _A3));;

let rec top_set (_A1, _A2, _A3) = uminus_set (bot_set (_A1, _A2, _A3));;

let rec le_of_comp
  acomp x y = (match acomp x y with Eq -> true | Lt -> true | Gt -> false);;

let rec lexordp_eq
  less xs ys = match less, xs, ys with
    less, x :: xs, y :: ys ->
      less x y || not (less y x) && lexordp_eq less xs ys
    | less, x :: xs, [] -> false
    | less, xs, [] -> null xs
    | less, [], ys -> true;;

let rec finite (_A1, _A2, _A3)
  = function
    Collect_set p ->
      of_phantom (finite_UNIV _A1) ||
        failwith "finite Collect_set"
          (fun _ -> finite (_A1, _A2, _A3) (Collect_set p))
    | Set_Monad xs -> true
    | Complement a ->
        (if of_phantom (finite_UNIV _A1) then true
          else (if finite (_A1, _A2, _A3) a then false
                 else failwith "finite Complement: infinite set"
                        (fun _ -> finite (_A1, _A2, _A3) (Complement a))))
    | RBT_set rbt ->
        (match ccompare _A3
          with None ->
            failwith "finite RBT_set: ccompare = None"
              (fun _ -> finite (_A1, _A2, _A3) (RBT_set rbt))
          | Some _ -> true)
    | DList_set dxs ->
        (match ceq _A2
          with None ->
            failwith "finite DList_set: ceq = None"
              (fun _ -> finite (_A1, _A2, _A3) (DList_set dxs))
          | Some _ -> true);;

let rec set_less_aux_Compl_fusion
  less proper_interval g1 g2 ao s1 s2 =
    (if has_next g1 s1
      then (let (x, s1a) = next g1 s1 in
             (if has_next g2 s2
               then (let (y, s2a) = next g2 s2 in
                      (if less x y
                        then proper_interval ao (Some x) ||
                               set_less_aux_Compl_fusion less proper_interval g1
                                 g2 (Some x) s1a s2
                        else (if less y x
                               then proper_interval ao (Some y) ||
                                      set_less_aux_Compl_fusion less
proper_interval g1 g2 (Some y) s1 s2a
                               else proper_interval ao (Some y))))
               else proper_interval ao (Some x) ||
                      set_less_aux_Compl_fusion less proper_interval g1 g2
                        (Some x) s1a s2))
      else (if has_next g2 s2
             then (let (y, s2a) = next g2 s2 in
                    proper_interval ao (Some y) ||
                      set_less_aux_Compl_fusion less proper_interval g1 g2
                        (Some y) s1 s2a)
             else proper_interval ao None));;

let rec compl_set_less_aux_fusion
  less proper_interval g1 g2 ao s1 s2 =
    has_next g1 s1 &&
      (has_next g2 s2 &&
        (let (x, s1a) = next g1 s1 in
         let (y, s2a) = next g2 s2 in
          (if less x y
            then not (proper_interval ao (Some x)) &&
                   compl_set_less_aux_fusion less proper_interval g1 g2 (Some x)
                     s1a s2
            else (if less y x
                   then not (proper_interval ao (Some y)) &&
                          compl_set_less_aux_fusion less proper_interval g1 g2
                            (Some y) s1 s2a
                   else not (proper_interval ao (Some y))))));;

let rec set_less_aux_Compl
  less proper_interval ao x3 x4 = match less, proper_interval, ao, x3, x4 with
    less, proper_interval, ao, x :: xs, y :: ys ->
      (if less x y
        then proper_interval ao (Some x) ||
               set_less_aux_Compl less proper_interval (Some x) xs (y :: ys)
        else (if less y x
               then proper_interval ao (Some y) ||
                      set_less_aux_Compl less proper_interval (Some y) (x :: xs)
                        ys
               else proper_interval ao (Some y)))
    | less, proper_interval, ao, x :: xs, [] ->
        proper_interval ao (Some x) ||
          set_less_aux_Compl less proper_interval (Some x) xs []
    | less, proper_interval, ao, [], y :: ys ->
        proper_interval ao (Some y) ||
          set_less_aux_Compl less proper_interval (Some y) [] ys
    | less, proper_interval, ao, [], [] -> proper_interval ao None;;

let rec compl_set_less_aux
  less proper_interval ao xs ys = match less, proper_interval, ao, xs, ys with
    less, proper_interval, ao, x :: xs, y :: ys ->
      (if less x y
        then not (proper_interval ao (Some x)) &&
               compl_set_less_aux less proper_interval (Some x) xs (y :: ys)
        else (if less y x
               then not (proper_interval ao (Some y)) &&
                      compl_set_less_aux less proper_interval (Some y) (x :: xs)
                        ys
               else not (proper_interval ao (Some y))))
    | less, proper_interval, ao, xs, [] -> false
    | less, proper_interval, ao, [], ys -> false;;

let rec lexord_fusion
  less g1 g2 s1 s2 =
    (if has_next g1 s1
      then (if has_next g2 s2
             then (let (x, s1a) = next g1 s1 in
                   let (y, s2a) = next g2 s2 in
                    less x y ||
                      not (less y x) && lexord_fusion less g1 g2 s1a s2a)
             else false)
      else has_next g2 s2);;

let rec lexordp
  less xs ys = match less, xs, ys with
    less, x :: xs, y :: ys -> less x y || not (less y x) && lexordp less xs ys
    | less, xs, [] -> false
    | less, [], ys -> not (null ys);;

let rec comp_of_ords
  le lt x y = (if lt x y then Lt else (if le x y then Eq else Gt));;

let rec ccompare_seta (_A1, _A2, _A3, _A4)
  = (match ccompare _A3.ccompare_cproper_interval with None -> None
      | Some _ ->
        Some (comp_of_ords (cless_eq_set (_A1, _A2, _A3, _A4))
               (cless_set (_A1, _A2, _A3, _A4))))
and cless_set (_A1, _A2, _A3, _A4)
  a b = match a, b with
    Complement (RBT_set rbt1), RBT_set rbt2 ->
      (match ccompare _A3.ccompare_cproper_interval
        with None ->
          failwith "cless_set (Complement RBT_set) RBT_set: ccompare = None"
            (fun _ ->
              cless_set (_A1, _A2, _A3, _A4) (Complement (RBT_set rbt1))
                (RBT_set rbt2))
        | Some c ->
          finite (_A1, _A2, _A3.ccompare_cproper_interval)
            (top_set (_A2, _A3.ccompare_cproper_interval, _A4)) &&
            compl_set_less_aux_fusion (lt_of_comp c) (cproper_interval _A3)
              rbt_keys_generator rbt_keys_generator None
              (init _A3.ccompare_cproper_interval rbt1)
              (init _A3.ccompare_cproper_interval rbt2))
    | RBT_set rbt1, Complement (RBT_set rbt2) ->
        (match ccompare _A3.ccompare_cproper_interval
          with None ->
            failwith "cless_set RBT_set (Complement RBT_set): ccompare = None"
              (fun _ ->
                cless_set (_A1, _A2, _A3, _A4) (RBT_set rbt1)
                  (Complement (RBT_set rbt2)))
          | Some c ->
            (if finite (_A1, _A2, _A3.ccompare_cproper_interval)
                  (top_set (_A2, _A3.ccompare_cproper_interval, _A4))
              then set_less_aux_Compl_fusion (lt_of_comp c)
                     (cproper_interval _A3) rbt_keys_generator
                     rbt_keys_generator None
                     (init _A3.ccompare_cproper_interval rbt1)
                     (init _A3.ccompare_cproper_interval rbt2)
              else true))
    | RBT_set rbta, RBT_set rbt ->
        (match ccompare _A3.ccompare_cproper_interval
          with None ->
            failwith "cless_set RBT_set RBT_set: ccompare = None"
              (fun _ ->
                cless_set (_A1, _A2, _A3, _A4) (RBT_set rbta) (RBT_set rbt))
          | Some c ->
            lexord_fusion (fun x y -> lt_of_comp c y x) rbt_keys_generator
              rbt_keys_generator (init _A3.ccompare_cproper_interval rbta)
              (init _A3.ccompare_cproper_interval rbt))
    | Complement a, Complement b ->
        (match ccompare _A3.ccompare_cproper_interval
          with None ->
            failwith "cless_set Complement Complement: ccompare = None"
              (fun _ ->
                cless_set (_A1, _A2, _A3, _A4) (Complement a) (Complement b))
          | Some _ -> lt_of_comp (the (ccompare_seta (_A1, _A2, _A3, _A4))) b a)
    | Complement a, b ->
        (match ccompare _A3.ccompare_cproper_interval
          with None ->
            failwith "cless_set Complement1: ccompare = None"
              (fun _ -> cless_set (_A1, _A2, _A3, _A4) (Complement a) b)
          | Some c ->
            (if finite (_A1, _A2, _A3.ccompare_cproper_interval) a &&
                  finite (_A1, _A2, _A3.ccompare_cproper_interval) b
              then finite (_A1, _A2, _A3.ccompare_cproper_interval)
                     (top_set (_A2, _A3.ccompare_cproper_interval, _A4)) &&
                     compl_set_less_aux (lt_of_comp c) (cproper_interval _A3)
                       None
                       (csorted_list_of_set (_A2, _A3.ccompare_cproper_interval)
                         a)
                       (csorted_list_of_set (_A2, _A3.ccompare_cproper_interval)
                         b)
              else failwith "cless_set Complement1: infinite set"
                     (fun _ ->
                       cless_set (_A1, _A2, _A3, _A4) (Complement a) b)))
    | a, Complement b ->
        (match ccompare _A3.ccompare_cproper_interval
          with None ->
            failwith "cless_set Complement2: ccompare = None"
              (fun _ -> cless_set (_A1, _A2, _A3, _A4) a (Complement b))
          | Some c ->
            (if finite (_A1, _A2, _A3.ccompare_cproper_interval) a &&
                  finite (_A1, _A2, _A3.ccompare_cproper_interval) b
              then (if finite (_A1, _A2, _A3.ccompare_cproper_interval)
                         (top_set (_A2, _A3.ccompare_cproper_interval, _A4))
                     then set_less_aux_Compl (lt_of_comp c)
                            (cproper_interval _A3) None
                            (csorted_list_of_set
                              (_A2, _A3.ccompare_cproper_interval) a)
                            (csorted_list_of_set
                              (_A2, _A3.ccompare_cproper_interval) b)
                     else true)
              else failwith "cless_set Complement2: infinite set"
                     (fun _ ->
                       cless_set (_A1, _A2, _A3, _A4) a (Complement b))))
    | a, b ->
        (match ccompare _A3.ccompare_cproper_interval
          with None ->
            failwith "cless_set: ccompare = None"
              (fun _ -> cless_set (_A1, _A2, _A3, _A4) a b)
          | Some c ->
            (if finite (_A1, _A2, _A3.ccompare_cproper_interval) a &&
                  finite (_A1, _A2, _A3.ccompare_cproper_interval) b
              then lexordp (fun x y -> lt_of_comp c y x)
                     (csorted_list_of_set (_A2, _A3.ccompare_cproper_interval)
                       a)
                     (csorted_list_of_set (_A2, _A3.ccompare_cproper_interval)
                       b)
              else failwith "cless_set: infinite set"
                     (fun _ -> cless_set (_A1, _A2, _A3, _A4) a b)))
and cless_eq_set (_A1, _A2, _A3, _A4)
  a b = match a, b with
    Complement (RBT_set rbt1), RBT_set rbt2 ->
      (match ccompare _A3.ccompare_cproper_interval
        with None ->
          failwith "cless_eq_set (Complement RBT_set) RBT_set: ccompare = None"
            (fun _ ->
              cless_eq_set (_A1, _A2, _A3, _A4) (Complement (RBT_set rbt1))
                (RBT_set rbt2))
        | Some c ->
          finite (_A1, _A2, _A3.ccompare_cproper_interval)
            (top_set (_A2, _A3.ccompare_cproper_interval, _A4)) &&
            compl_set_less_eq_aux_fusion (lt_of_comp c) (cproper_interval _A3)
              rbt_keys_generator rbt_keys_generator None
              (init _A3.ccompare_cproper_interval rbt1)
              (init _A3.ccompare_cproper_interval rbt2))
    | RBT_set rbt1, Complement (RBT_set rbt2) ->
        (match ccompare _A3.ccompare_cproper_interval
          with None ->
            failwith
              "cless_eq_set RBT_set (Complement RBT_set): ccompare = None"
              (fun _ ->
                cless_eq_set (_A1, _A2, _A3, _A4) (RBT_set rbt1)
                  (Complement (RBT_set rbt2)))
          | Some c ->
            (if finite (_A1, _A2, _A3.ccompare_cproper_interval)
                  (top_set (_A2, _A3.ccompare_cproper_interval, _A4))
              then set_less_eq_aux_Compl_fusion (lt_of_comp c)
                     (cproper_interval _A3) rbt_keys_generator
                     rbt_keys_generator None
                     (init _A3.ccompare_cproper_interval rbt1)
                     (init _A3.ccompare_cproper_interval rbt2)
              else true))
    | RBT_set rbta, RBT_set rbt ->
        (match ccompare _A3.ccompare_cproper_interval
          with None ->
            failwith "cless_eq_set RBT_set RBT_set: ccompare = None"
              (fun _ ->
                cless_eq_set (_A1, _A2, _A3, _A4) (RBT_set rbta) (RBT_set rbt))
          | Some c ->
            lexord_eq_fusion (fun x y -> lt_of_comp c y x) rbt_keys_generator
              rbt_keys_generator (init _A3.ccompare_cproper_interval rbta)
              (init _A3.ccompare_cproper_interval rbt))
    | Complement a, Complement b ->
        (match ccompare _A3.ccompare_cproper_interval
          with None ->
            failwith "cless_eq_set Complement Complement: ccompare = None"
              (fun _ ->
                le_of_comp (the (ccompare_seta (_A1, _A2, _A3, _A4)))
                  (Complement a) (Complement b))
          | Some _ -> cless_eq_set (_A1, _A2, _A3, _A4) b a)
    | Complement a, b ->
        (match ccompare _A3.ccompare_cproper_interval
          with None ->
            failwith "cless_eq_set Complement1: ccompare = None"
              (fun _ -> cless_eq_set (_A1, _A2, _A3, _A4) (Complement a) b)
          | Some c ->
            (if finite (_A1, _A2, _A3.ccompare_cproper_interval) a &&
                  finite (_A1, _A2, _A3.ccompare_cproper_interval) b
              then finite (_A1, _A2, _A3.ccompare_cproper_interval)
                     (top_set (_A2, _A3.ccompare_cproper_interval, _A4)) &&
                     compl_set_less_eq_aux (lt_of_comp c) (cproper_interval _A3)
                       None
                       (csorted_list_of_set (_A2, _A3.ccompare_cproper_interval)
                         a)
                       (csorted_list_of_set (_A2, _A3.ccompare_cproper_interval)
                         b)
              else failwith "cless_eq_set Complement1: infinite set"
                     (fun _ ->
                       cless_eq_set (_A1, _A2, _A3, _A4) (Complement a) b)))
    | a, Complement b ->
        (match ccompare _A3.ccompare_cproper_interval
          with None ->
            failwith "cless_eq_set Complement2: ccompare = None"
              (fun _ -> cless_eq_set (_A1, _A2, _A3, _A4) a (Complement b))
          | Some c ->
            (if finite (_A1, _A2, _A3.ccompare_cproper_interval) a &&
                  finite (_A1, _A2, _A3.ccompare_cproper_interval) b
              then (if finite (_A1, _A2, _A3.ccompare_cproper_interval)
                         (top_set (_A2, _A3.ccompare_cproper_interval, _A4))
                     then set_less_eq_aux_Compl (lt_of_comp c)
                            (cproper_interval _A3) None
                            (csorted_list_of_set
                              (_A2, _A3.ccompare_cproper_interval) a)
                            (csorted_list_of_set
                              (_A2, _A3.ccompare_cproper_interval) b)
                     else true)
              else failwith "cless_eq_set Complement2: infinite set"
                     (fun _ ->
                       cless_eq_set (_A1, _A2, _A3, _A4) a (Complement b))))
    | a, b ->
        (match ccompare _A3.ccompare_cproper_interval
          with None ->
            failwith "cless_eq_set: ccompare = None"
              (fun _ -> cless_eq_set (_A1, _A2, _A3, _A4) a b)
          | Some c ->
            (if finite (_A1, _A2, _A3.ccompare_cproper_interval) a &&
                  finite (_A1, _A2, _A3.ccompare_cproper_interval) b
              then lexordp_eq (fun x y -> lt_of_comp c y x)
                     (csorted_list_of_set (_A2, _A3.ccompare_cproper_interval)
                       a)
                     (csorted_list_of_set (_A2, _A3.ccompare_cproper_interval)
                       b)
              else failwith "cless_eq_set: infinite set"
                     (fun _ -> cless_eq_set (_A1, _A2, _A3, _A4) a b)));;

let rec ccompare_set (_A1, _A2, _A3, _A4) =
  ({ccompare = ccompare_seta (_A1, _A2, _A3, _A4)} : 'a set ccompare);;

let rec equal_lista _A
  x0 x1 = match x0, x1 with [], x21 :: x22 -> false
    | x21 :: x22, [] -> false
    | x21 :: x22, y21 :: y22 -> eq _A x21 y21 && equal_lista _A x22 y22
    | [], [] -> true;;

let rec equal_list _A = ({equal = equal_lista _A} : ('a list) equal);;

type 'a poly = Poly of 'a list;;

type real_alg_2 = Rational of rat |
  Irrational of nat * (int poly * (rat * rat));;

type real_alg_3 = Real_Alg_Invariant of real_alg_2;;

type real_alg = Real_Alg_Quotient of real_alg_3;;

let rec rep_real_alg_3 (Real_Alg_Invariant x) = x;;

let rec coeffs _A (Poly x) = x;;

let rec equal_polya (_A1, _A2)
  p q = equal_lista _A2 (coeffs _A1 p) (coeffs _A1 q);;

let rec equal_2
  x0 x1 = match x0, x1 with Rational r, Rational q -> equal_rata r q
    | Irrational (n, (p, uu)), Irrational (m, (q, uv)) ->
        equal_polya (zero_int, equal_int) p q && equal_nata n m
    | Rational r, Irrational (uw, yy) -> false
    | Irrational (ux, xx), Rational q -> false;;

let rec equal_3 xa xc = equal_2 (rep_real_alg_3 xa) (rep_real_alg_3 xc);;

let rec equal_real_alg
  (Real_Alg_Quotient xc) (Real_Alg_Quotient xa) = equal_3 xc xa;;

type real = Real_of of real_alg;;

let rec equal_reala (Real_of x) (Real_of y) = equal_real_alg x y;;

let equal_real = ({equal = equal_reala} : real equal);;

let rec binary_power _A
  x n = (if equal_nata n zero_nata then one _A.power_monoid_mult.one_power
          else (let (d, r) = divmod_nat n (nat_of_integer (Z.of_int 2)) in
                let reca =
                  binary_power _A (times _A.power_monoid_mult.times_power x x) d
                  in
                 (if equal_nata r zero_nata then reca
                   else times _A.power_monoid_mult.times_power reca x)));;

let rec dropWhile p x1 = match p, x1 with p, [] -> []
                    | p, x :: xs -> (if p x then dropWhile p xs else x :: xs);;

let rec rev xs = fold (fun a b -> a :: b) xs [];;

let rec strip_while p = comp (comp rev (dropWhile p)) rev;;

let rec poly_of_list (_A1, _A2)
  asa = Poly (strip_while
               (eq _A2 (zero _A1.monoid_add_comm_monoid_add.zero_monoid_add))
               asa);;

let rec map f x1 = match f, x1 with f, [] -> []
              | f, x21 :: x22 -> f x21 :: map f x22;;

let rec zip xs ys = match xs, ys with x :: xs, y :: ys -> (x, y) :: zip xs ys
              | xs, [] -> []
              | [], ys -> [];;

let rec upt i j = (if less_nat i j then i :: upt (suc i) j else []);;

let rec poly_mult_rat_main (_A1, _A2)
  n d f =
    (let fs =
       coeffs
         _A2.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
         f
       in
     let k = size_list fs in
      poly_of_list
        (_A2.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.comm_monoid_add_semiring_0,
          _A1)
        (map (fun (fi, i) ->
               times _A2.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd
                 (times
                   _A2.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd
                   fi (binary_power
                        _A2.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_numeral_semiring_1.monoid_mult_semiring_numeral
                        d i))
                 (binary_power
                   _A2.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_numeral_semiring_1.monoid_mult_semiring_numeral
                   n (minus_nata k (suc i))))
          (zip fs (upt zero_nata k))));;

let rec poly_mult_rat
  r p = (let (n, d) = quotient_of r in
          poly_mult_rat_main (equal_int, idom_int) n d p);;

let rec map_poly _B (_A1, _A2)
  f p = Poly (strip_while (eq _A2 (zero _A1)) (map f (coeffs _B p)));;

let rec sdiv_poly (_A1, _A2)
  p a = map_poly
          _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
          (_A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
            _A1)
          (fun c ->
            divide _A2.semidom_divide_idom_divide.divide_semidom_divide c a)
          p;;

let rec nth
  (x :: xs) n =
    (if equal_nata n zero_nata then x else nth xs (minus_nata n one_nata));;

let rec nth_default
  dflt xs n = (if less_nat n (size_list xs) then nth xs n else dflt);;

let rec coeffa _A p = nth_default (zero _A) (coeffs _A p);;

let rec foldr f x1 = match f, x1 with f, [] -> id
                | f, x :: xs -> comp (f x) (foldr f xs);;

let rec fold_coeffs _A f p = foldr f (coeffs _A p);;

let rec content _A
  p = fold_coeffs _A.comm_monoid_gcd_semiring_gcd.gcd_comm_monoid_gcd.zero_gcd
        (gcda _A.comm_monoid_gcd_semiring_gcd.gcd_comm_monoid_gcd) p
        (zero _A.comm_monoid_gcd_semiring_gcd.gcd_comm_monoid_gcd.zero_gcd);;

let rec degreea _A p = minus_nata (size_list (coeffs _A p)) one_nata;;

let rec cf_pos_poly
  f = (let c = content semiring_gcd_int f in
       let a = times_inta (sgn_int (coeffa zero_int f (degreea zero_int f))) c
         in
        sdiv_poly (equal_int, idom_divide_int) f a);;

let rec of_int a = Frct (a, one_inta);;

let rec tighten_poly_bounds
  p l r sr =
    (let m = divide_rata (plus_rata l r) (of_int (Int_of_integer (Z.of_int 2)))
       in
     let sm =
       sgn_rata
         (fold_coeffs zero_int
           (fun a b -> plus_rata (of_int a) (times_rata m b)) p zero_rata)
       in
      (if equal_rata sm sr then (l, (m, sm)) else (m, (r, sr))));;

let rec tighten_poly_bounds_epsilon
  p x l r sr =
    (if less_eq_rat (minus_rata r l) x then (l, (r, sr))
      else (let (la, (a, b)) = tighten_poly_bounds p l r sr in
             tighten_poly_bounds_epsilon p x la a b));;

let rec tighten_poly_bounds_for_x
  p x l r sr =
    (if less_rat x l || less_rat r x then (l, (r, sr))
      else (let (la, (a, b)) = tighten_poly_bounds p l r sr in
             tighten_poly_bounds_for_x p x la a b));;

let rec normalize_bounds_1_main
  eps rai =
    (let (p, (l, r)) = rai in
     let (la, (ra, sr)) =
       tighten_poly_bounds_epsilon p eps l r
         (sgn_rata
           (fold_coeffs zero_int
             (fun a b -> plus_rata (of_int a) (times_rata r b)) p zero_rata))
       in
     let fr = of_int (floor_rat ra) in
     let (lb, (rb, _)) = tighten_poly_bounds_for_x p fr la ra sr in
      (p, (lb, rb)));;

let rec fract a b = Frct (normalize (a, b));;

let real_alg_precision : rat = fract one_inta (Int_of_integer (Z.of_int 2));;

let rec normalize_bounds_1 x = normalize_bounds_1_main real_alg_precision x;;

let rec poly_real_alg_1 (p, (uu, uv)) = p;;

type root_info = Root_Info of (rat -> rat -> nat) * (rat -> nat);;

let rec number_root (Root_Info (x1, x2)) = x2;;

let rec dvd (_A1, _A2)
  a b = eq _A1
          (modulo _A2.semiring_modulo_semidom_modulo.modulo_semiring_modulo b a)
          (zero _A2.algebraic_semidom_semidom_modulo.semidom_divide_algebraic_semidom.semidom_semidom_divide.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero);;

let rec poly_neg_number_rootat
  p = (if dvd (equal_nat, semidom_modulo_nat) (nat_of_integer (Z.of_int 2))
            (degreea zero_rat p)
        then sgn_rata (coeffa zero_rat p (degreea zero_rat p))
        else uminus_rata (sgn_rata (coeffa zero_rat p (degreea zero_rat p))));;

let rec remdups_adj _A
  = function [] -> []
    | [x] -> [x]
    | x :: y :: xs ->
        (if eq _A x y then remdups_adj _A (x :: xs)
          else x :: remdups_adj _A (y :: xs));;

let rec sign_changes_neg_number_rootat
  ps = minus_nata
         (size_list
           (remdups_adj equal_rat
             (filtera (fun x -> not (equal_rata x zero_rata))
               (map poly_neg_number_rootat ps))))
         one_nata;;

let rec horner_sum _B
  f a xs =
    foldr (fun x b ->
            plus _B.semiring_0_comm_semiring_0.comm_monoid_add_semiring_0.monoid_add_comm_monoid_add.semigroup_add_monoid_add.plus_semigroup_add
              (f x)
              (times
                _B.semiring_0_comm_semiring_0.mult_zero_semiring_0.times_mult_zero
                a b))
      xs (zero _B.semiring_0_comm_semiring_0.mult_zero_semiring_0.zero_mult_zero);;

let rec poly _A
  p a = horner_sum _A id a
          (coeffs
            _A.semiring_0_comm_semiring_0.mult_zero_semiring_0.zero_mult_zero
            p);;

let rec sign_changes_rat
  ps x =
    minus_nata
      (size_list
        (remdups_adj equal_rat
          (filtera (fun xa -> not (equal_rata xa zero_rata))
            (map (fun p -> sgn_rata (poly comm_semiring_0_rat p x)) ps))))
      one_nata;;

let rec uminus_polya _A
  p = Poly (map (uminus _A.group_add_ab_group_add.uminus_group_add)
             (coeffs
               _A.group_add_ab_group_add.monoid_add_group_add.zero_monoid_add
               p));;

let rec minus_poly_rev_list _A
  xs x1 = match xs, x1 with
    x :: xs, y :: ys ->
      minus _A.minus_group_add x y :: minus_poly_rev_list _A xs ys
    | xs, [] -> xs
    | [], y :: ys -> [];;

let rec tla = function [] -> []
              | x21 :: x22 -> x22;;

let rec hda (x21 :: x22) = x21;;

let rec mod_poly_one_main_list (_A1, _A2)
  r d n =
    (if equal_nata n zero_nata then r
      else (let a = hda r in
            let rr =
              tla (if eq _A1 a
                        (zero _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
                    then r
                    else minus_poly_rev_list
                           _A2.ring_1_comm_ring_1.neg_numeral_ring_1.group_add_neg_numeral
                           r (map (times
                                    _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd
                                    a)
                               d))
              in
             mod_poly_one_main_list (_A1, _A2) rr d (minus_nata n one_nata)));;

let rec last (x :: xs) = (if null xs then x else last xs);;

let rec modulo_poly (_A1, _A2)
  f g = (let cg =
           coeffs
             _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
             g
           in
          (if null cg then f
            else (let cf =
                    coeffs
                      _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
                      f
                    in
                  let ilc =
                    inverse _A1.division_ring_field.inverse_division_ring
                      (last cg)
                    in
                  let ch =
                    map (times
                          _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd
                          ilc)
                      cg
                    in
                  let r =
                    mod_poly_one_main_list
                      (_A2, _A1.idom_divide_field.idom_idom_divide.comm_ring_1_idom)
                      (rev cf) (rev ch)
                      (minus_nata (plus_nata one_nata (size_list cf))
                        (size_list cg))
                    in
                   poly_of_list
                     (_A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.comm_monoid_add_semiring_0,
                       _A2)
                     (rev r))));;

let rec sturm_aux_rat
  p q = (if equal_nata (degreea zero_rat q) zero_nata then [p; q]
          else p :: sturm_aux_rat q
                      (uminus_polya ab_group_add_rat
                        (modulo_poly (field_rat, equal_rat) p q)));;

let rec cCons (_A1, _A2)
  x xs = (if null xs && eq _A2 x (zero _A1) then [] else x :: xs);;

let rec pderiv_coeffs_code (_A1, _A2, _A3)
  f x1 = match f, x1 with
    f, x :: xs ->
      cCons (_A2.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
              _A1)
        (times
          _A2.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd f
          x)
        (pderiv_coeffs_code (_A1, _A2, _A3)
          (plus _A2.semiring_1_comm_semiring_1.semiring_numeral_semiring_1.numeral_semiring_numeral.semigroup_add_numeral.plus_semigroup_add
            f (one _A2.semiring_1_comm_semiring_1.semiring_numeral_semiring_1.numeral_semiring_numeral.one_numeral))
          xs)
    | f, [] -> [];;

let rec pderiv_coeffs (_A1, _A2, _A3)
  xs = pderiv_coeffs_code (_A1, _A2, _A3)
         (one _A2.semiring_1_comm_semiring_1.semiring_numeral_semiring_1.numeral_semiring_numeral.one_numeral)
         (tla xs);;

let rec pderiv (_A1, _A2, _A3)
  p = Poly (pderiv_coeffs (_A1, _A2, _A3)
             (coeffs
               _A2.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
               p));;

let rec sturm_rat
  p = sturm_aux_rat p
        (pderiv (equal_rat, comm_semiring_1_rat, semiring_no_zero_divisors_rat)
          p);;

let rec root_info
  p = (if equal_nata (degreea zero_int p) one_nata
        then (let x =
                fract (uminus_inta
                        (match coeffs zero_int p with [] -> zero_inta
                          | x :: _ -> x))
                  (coeffa zero_int p one_nata)
                in
               Root_Info
                 ((fun l r ->
                    (if less_eq_rat l x && less_eq_rat x r then one_nata
                      else zero_nata)),
                   (fun b ->
                     (if less_eq_rat x b then one_nata else zero_nata))))
        else (let rp = map_poly zero_int (zero_rat, equal_rat) of_int p in
              let ps = sturm_rat rp in
               Root_Info
                 ((fun a b ->
                    minus_nata (sign_changes_rat ps a) (sign_changes_rat ps b)),
                   (fun a ->
                     minus_nata (sign_changes_neg_number_rootat ps)
                       (sign_changes_rat ps a)))));;

let rec real_alg_2
  rai = (let p = poly_real_alg_1 rai in
          (if equal_nata (degreea zero_int p) one_nata
            then Rational
                   (fract
                     (uminus_inta
                       (match coeffs zero_int p with [] -> zero_inta
                         | x :: _ -> x))
                     (coeffa zero_int p one_nata))
            else (let (pa, (l, r)) = normalize_bounds_1 rai in
                   Irrational (number_root (root_info p) r, (pa, (l, r))))));;

let rec mult_rat_1_pos
  r1 (p2, (l2, r2)) =
    real_alg_2
      (cf_pos_poly (poly_mult_rat r1 p2),
        (times_rata l2 r1, times_rata r2 r1));;

let rec abs_int_poly
  p = (if less_int (coeffa zero_int p (degreea zero_int p)) zero_inta
        then uminus_polya ab_group_add_int p else p);;

let rec zero_polya _A = Poly [];;

let rec pCons (_A1, _A2) a p = Poly (cCons (_A1, _A2) a (coeffs _A1 p));;

let rec poly_uminus_inner (_A1, _A2)
  = function
    [] -> zero_polya
            _A2.semiring_1_cancel_ring_1.semiring_1_semiring_1_cancel.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
    | [a] ->
        pCons (_A2.semiring_1_cancel_ring_1.semiring_1_semiring_1_cancel.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
                _A1)
          a (zero_polya
              _A2.semiring_1_cancel_ring_1.semiring_1_semiring_1_cancel.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
    | a :: b :: cs ->
        pCons (_A2.semiring_1_cancel_ring_1.semiring_1_semiring_1_cancel.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
                _A1)
          a (pCons
              (_A2.semiring_1_cancel_ring_1.semiring_1_semiring_1_cancel.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
                _A1)
              (uminus
                _A2.neg_numeral_ring_1.group_add_neg_numeral.uminus_group_add b)
              (poly_uminus_inner (_A1, _A2) cs));;

let rec poly_uminus (_A1, _A2)
  p = poly_uminus_inner (_A1, _A2)
        (coeffs
          _A2.semiring_1_cancel_ring_1.semiring_1_semiring_1_cancel.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
          p);;

let rec uminus_1
  (p, (l, r)) =
    (abs_int_poly (poly_uminus (equal_int, ring_1_int) p),
      (uminus_rata r, uminus_rata l));;

let rec uminus_2 = function Rational r -> Rational (uminus_rata r)
                   | Irrational (n, x) -> real_alg_2 (uminus_1 x);;

let rec mult_rat_1
  x y = (if less_rat x zero_rata
          then uminus_2 (mult_rat_1_pos (uminus_rata x) y)
          else (if equal_rata x zero_rata then Rational zero_rata
                 else mult_rat_1_pos x y));;

let rec l_r (Root_Info (x1, x2)) = x1;;

let rec select_correct_factor_main
  bnd_update bnd_get bnd todo old l r n =
    (match todo
      with [] ->
        (if equal_nata n one_nata then (hda old, (l, r))
          else (let bnda = bnd_update bnd in
                let (la, ra) = bnd_get bnda in
                 select_correct_factor_main bnd_update bnd_get bnda old [] la ra
                   zero_nata))
      | (p, ri) :: todoa ->
        (let m = l_r ri l r in
          (if equal_nata m zero_nata
            then select_correct_factor_main bnd_update bnd_get bnd todoa old l r
                   n
            else select_correct_factor_main bnd_update bnd_get bnd todoa
                   ((p, ri) :: old) l r (plus_nata n m))));;

let rec select_correct_factor
  bnd_update bnd_get init polys =
    (let (l, r) = bnd_get init in
      select_correct_factor_main bnd_update bnd_get init polys [] l r
        zero_nata);;

type int_poly_factorization_algorithm =
  Abs_int_poly_factorization_algorithm of (int poly -> int poly list);;

type 'a arith_ops_record =
  Arith_Ops_Record of
    'a * 'a * ('a -> 'a -> 'a) * ('a -> 'a -> 'a) * ('a -> 'a -> 'a) *
      ('a -> 'a) * ('a -> 'a -> 'a) * ('a -> 'a) * ('a -> 'a -> 'a) *
      ('a -> 'a) * ('a -> 'a) * (int -> 'a) * ('a -> int) * ('a -> bool);;

let rec onea
  (Arith_Ops_Record
    (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14))
    = x2;;

let karatsuba_lower_bound : nat = nat_of_integer (Z.of_int 7);;

let rec zeroa
  (Arith_Ops_Record
    (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14))
    = x1;;

let rec poly_of_list_i _A ops = strip_while (eq _A (zeroa ops));;

let rec uminusa
  (Arith_Ops_Record
    (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14))
    = x6;;

let rec minusa
  (Arith_Ops_Record
    (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14))
    = x5;;

let rec coeffs_minus_i
  ops xs x2 = match ops, xs, x2 with
    ops, x :: xs, y :: ys -> minusa ops x y :: coeffs_minus_i ops xs ys
    | ops, xs, [] -> xs
    | ops, [], v :: va -> map (uminusa ops) (v :: va);;

let rec replicate
  n x = (if equal_nata n zero_nata then []
          else x :: replicate (minus_nata n one_nata) x);;

let rec monom_mult_i
  ops n xs = (if null xs then xs else replicate n (zeroa ops) @ xs);;

let rec cCons_i _A
  ops x xs = (if null xs && eq _A x (zeroa ops) then [] else x :: xs);;

let rec minus_poly_i _A
  ops xs x2 = match ops, xs, x2 with
    ops, x :: xs, y :: ys ->
      cCons_i _A ops (minusa ops x y) (minus_poly_i _A ops xs ys)
    | ops, xs, [] -> xs
    | ops, [], v :: va -> map (uminusa ops) (v :: va);;

let rec plusa
  (Arith_Ops_Record
    (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14))
    = x3;;

let rec plus_poly_i _A
  ops xs x2 = match ops, xs, x2 with
    ops, x :: xs, y :: ys ->
      cCons_i _A ops (plusa ops x y) (plus_poly_i _A ops xs ys)
    | ops, xs, [] -> xs
    | ops, [], v :: va -> v :: va;;

let rec split_at
  n x1 = match n, x1 with
    n, x :: xs ->
      (if equal_nata n zero_nata then ([], x :: xs)
        else (let (bef, a) = split_at (minus_nata n one_nata) xs in
               (x :: bef, a)))
    | n, [] -> ([], []);;

let rec timesa
  (Arith_Ops_Record
    (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14))
    = x4;;

let rec smult_i _A
  ops a pp =
    (if eq _A a (zeroa ops) then []
      else strip_while (eq _A (zeroa ops)) (map (timesa ops a) pp));;

let rec karatsuba_main_i _A
  ops f n g m =
    (if less_eq_nat n karatsuba_lower_bound ||
          less_eq_nat m karatsuba_lower_bound
      then (let ff = poly_of_list_i _A ops f in
             foldr (fun a p ->
                     plus_poly_i _A ops (smult_i _A ops a ff)
                       (cCons_i _A ops (zeroa ops) p))
               g [])
      else (let n2 = divide_nata n (nat_of_integer (Z.of_int 2)) in
             (if less_nat n2 m
               then (let (f0, f1) = split_at n2 f in
                     let (g0, g1) = split_at n2 g in
                     let p1 =
                       karatsuba_main_i _A ops f1 (minus_nata n n2) g1
                         (minus_nata m n2)
                       in
                     let p2 =
                       karatsuba_main_i _A ops (coeffs_minus_i ops f1 f0) n2
                         (coeffs_minus_i ops g1 g0) n2
                       in
                     let p3 = karatsuba_main_i _A ops f0 n2 g0 n2 in
                      plus_poly_i _A ops (monom_mult_i ops (plus_nata n2 n2) p1)
                        (plus_poly_i _A ops
                          (monom_mult_i ops n2
                            (plus_poly_i _A ops (minus_poly_i _A ops p1 p2) p3))
                          p3))
               else (let (f0, f1) = split_at n2 f in
                     let p1 = karatsuba_main_i _A ops f1 (minus_nata n n2) g m
                       in
                     let a = karatsuba_main_i _A ops f0 n2 g m in
                      plus_poly_i _A ops (monom_mult_i ops n2 p1) a))));;

let rec times_poly_i _A
  ops f g =
    (let n = size_list f in
     let m = size_list g in
      (if less_eq_nat n karatsuba_lower_bound ||
            less_eq_nat m karatsuba_lower_bound
        then (if less_eq_nat n m
               then foldr (fun a p ->
                            plus_poly_i _A ops (smult_i _A ops a g)
                              (cCons_i _A ops (zeroa ops) p))
                      f []
               else foldr (fun a p ->
                            plus_poly_i _A ops (smult_i _A ops a f)
                              (cCons_i _A ops (zeroa ops) p))
                      g [])
        else (if less_eq_nat n m then karatsuba_main_i _A ops g m f n
               else karatsuba_main_i _A ops f n g m)));;

let rec power_poly_f_mod_i _A
  ff_ops modulus a n =
    (if equal_nata n zero_nata then modulus [onea ff_ops]
      else (let (d, r) = divmod_nat n (nat_of_integer (Z.of_int 2)) in
            let reca =
              power_poly_f_mod_i _A ff_ops modulus
                (modulus (times_poly_i _A ff_ops a a)) d
              in
             (if equal_nata r zero_nata then reca
               else modulus (times_poly_i _A ff_ops reca a))));;

let rec inversea
  (Arith_Ops_Record
    (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14))
    = x8;;

let rec minus_poly_rev_list_i
  ops xs x2 = match ops, xs, x2 with
    ops, x :: xs, y :: ys -> minusa ops x y :: minus_poly_rev_list_i ops xs ys
    | ops, xs, [] -> xs
    | ops, [], y :: ys -> [];;

let rec mod_poly_one_main_i _A
  ops r d n =
    (if equal_nata n zero_nata then r
      else (let a = hda r in
            let rr =
              tla (if eq _A a (zeroa ops) then r
                    else minus_poly_rev_list_i ops r (map (timesa ops a) d))
              in
             mod_poly_one_main_i _A ops rr d (minus_nata n one_nata)));;

let rec mod_field_poly_i _A
  ops cf cg =
    (if null cg then cf
      else (let ilc = inversea ops (last cg) in
            let ch = map (timesa ops ilc) cg in
            let r =
              mod_poly_one_main_i _A ops (rev cf) (rev ch)
                (minus_nata (plus_nata one_nata (size_list cf)) (size_list cg))
              in
             poly_of_list_i _A ops (rev r)));;

let rec divmod_poly_one_main_i _A
  ops q r d n =
    (if equal_nata n zero_nata then (q, r)
      else (let a = hda r in
            let qqq = cCons_i _A ops a q in
            let rr =
              tla (if eq _A a (zeroa ops) then r
                    else minus_poly_rev_list_i ops r (map (timesa ops a) d))
              in
             divmod_poly_one_main_i _A ops qqq rr d (minus_nata n one_nata)));;

let rec div_field_poly_i _A
  ops cf cg =
    (if null cg then []
      else (let ilc = inversea ops (last cg) in
            let ch = map (timesa ops ilc) cg in
            let q =
              fst (divmod_poly_one_main_i _A ops [] (rev cf) (rev ch)
                    (minus_nata (plus_nata one_nata (size_list cf))
                      (size_list cg)))
              in
             poly_of_list_i _A ops (map (timesa ops ilc) q)));;

let rec normalizeb
  (Arith_Ops_Record
    (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14))
    = x10;;

let rec moduloa
  (Arith_Ops_Record
    (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14))
    = x9;;

let rec gcd_eucl_i _A
  ops a b =
    (if eq _A b (zeroa ops) then normalizeb ops a
      else gcd_eucl_i _A ops b (moduloa ops a b));;

let rec of_intb
  (Arith_Ops_Record
    (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14))
    = x12;;

let rec unit_factora
  (Arith_Ops_Record
    (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14))
    = x11;;

let rec lead_coeff_i
  ops pp = (match pp with [] -> zeroa ops | _ :: _ -> last pp);;

let rec unit_factor_poly_i _A
  ops xs = cCons_i _A ops (unit_factora ops (lead_coeff_i ops xs)) [];;

let rec normalize_poly_i _A
  ops xs =
    smult_i _A ops (inversea ops (unit_factora ops (lead_coeff_i ops xs))) xs;;

let rec dp
  (Arith_Ops_Record
    (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14))
    = x14;;

let rec no_leading p xs = (if not (null xs) then not (p (hda xs)) else true);;

let rec is_poly _A
  ops xs = list_all (dp ops) xs && no_leading (eq _A (zeroa ops)) (rev xs);;

let rec poly_ops _A
  ops = Arith_Ops_Record
          ([], [onea ops], plus_poly_i _A ops, times_poly_i _A ops,
            minus_poly_i _A ops, map (uminusa ops), div_field_poly_i _A ops,
            (fun _ -> []), mod_field_poly_i _A ops, normalize_poly_i _A ops,
            unit_factor_poly_i _A ops,
            (fun i -> (if equal_inta i zero_inta then [] else [of_intb ops i])),
            (fun _ -> zero_inta), is_poly _A ops);;

let rec gcd_poly_i _A ops = gcd_eucl_i (equal_list _A) (poly_ops _A ops);;

let rec degree_i pp = minus_nata (size_list pp) one_nata;;

let rec dist_degree_factorize_main_i _A
  p ff_ops ze on dv v w d res =
    (if equal_lista _A v [on] then res
      else (if less_nat dv (plus_nata d d) then (dv, v) :: res
             else (let wa =
                     power_poly_f_mod_i _A ff_ops
                       (fun f -> mod_field_poly_i _A ff_ops f v) w (nat p)
                     in
                   let da = suc d in
                   let gd =
                     gcd_poly_i _A ff_ops (minus_poly_i _A ff_ops wa [ze; on]) v
                     in
                    (if equal_lista _A gd [on]
                      then dist_degree_factorize_main_i _A p ff_ops ze on dv v
                             wa da res
                      else (let va = div_field_poly_i _A ff_ops v gd in
                             dist_degree_factorize_main_i _A p ff_ops ze on
                               (degree_i va) va
                               (mod_field_poly_i _A ff_ops wa va) da
                               ((da, gd) :: res))))));;

let rec distinct_degree_factorization_i _A
  p ff_ops f =
    (let ze = zeroa ff_ops in
     let on = onea ff_ops in
      (if equal_nata (degree_i f) one_nata then [(one_nata, f)]
        else dist_degree_factorize_main_i _A p ff_ops ze on (degree_i f) f
               [ze; on] zero_nata []));;

let rec int_of_nat n = Int_of_integer (integer_of_nat n);;

let rec partition
  p x1 = match p, x1 with p, [] -> ([], [])
    | p, x :: xs ->
        (let (yes, no) = partition p xs in
          (if p x then (x :: yes, no) else (yes, x :: no)));;

let rec maps f x1 = match f, x1 with f, [] -> []
               | f, x :: xs -> f x @ maps f xs;;

let rec berlekamp_factorization_main_i _A
  p ff_ops ze on d divs x6 n = match p, ff_ops, ze, on, d, divs, x6, n with
    p, ff_ops, ze, on, d, divs, v :: vs, n ->
      (if equal_lista _A v [on]
        then berlekamp_factorization_main_i _A p ff_ops ze on d divs vs n
        else (if equal_nata (size_list divs) n then divs
               else (let of_int = of_intb ff_ops in
                     let facts =
                       filtera (fun w -> not (equal_lista _A w [on]))
                         (maps (fun u ->
                                 map (fun s ->
                                       gcd_poly_i _A ff_ops u
 (minus_poly_i _A ff_ops v
   (if equal_nata s zero_nata then [] else [of_int (int_of_nat s)])))
                                   (upt zero_nata (nat p)))
                           divs)
                       in
                     let (lin, nonlin) =
                       partition (fun q -> equal_nata (degree_i q) d) facts in
                      lin @ berlekamp_factorization_main_i _A p ff_ops ze on d
                              nonlin vs (minus_nata n (size_list lin)))))
    | p, ff_ops, ze, on, d, divs, [], n -> divs;;

let rec power_polys_i _A
  ff_ops mul_p u curr_p i =
    (if equal_nata i zero_nata then []
      else curr_p ::
             power_polys_i _A ff_ops mul_p u
               (mod_field_poly_i _A ff_ops (times_poly_i _A ff_ops curr_p mul_p)
                 u)
               (minus_nata i one_nata));;

type 'a iarray = IArray of 'a list;;

type 'a x_a_mat_impl_option_x_x_x_a_iarray_iarray_nat_prod_nat_prod_option =
  Abs_x_a_mat_impl_option_x_x_x_a_iarray_iarray_nat_prod_nat_prod_option of
    (nat * (nat * 'a iarray iarray)) option;;

let rec rep_x_a_mat_impl_option_x_x_x_a_iarray_iarray_nat_prod_nat_prod_option
  (Abs_x_a_mat_impl_option_x_x_x_a_iarray_iarray_nat_prod_nat_prod_option x) =
    x;;

type 'a mat_impl = Abs_mat_impl of (nat * (nat * 'a iarray iarray));;

let rec rep_mat_impl (Abs_mat_impl x) = x;;

let rec sel21a
  xa = Abs_mat_impl
         (match
           rep_x_a_mat_impl_option_x_x_x_a_iarray_iarray_nat_prod_nat_prod_option
             xa
           with None -> rep_mat_impl (failwith "undefined") | Some x2 -> x2);;

let rec dis1a
  xa = (match
         rep_x_a_mat_impl_option_x_x_x_a_iarray_iarray_nat_prod_nat_prod_option
           xa
         with None -> true | Some _ -> false);;

let rec rep_isoma x = (if dis1a x then None else Some (sel21a x));;

let rec mat_of_rows_list_impl_aux
  xb xc =
    Abs_x_a_mat_impl_option_x_x_x_a_iarray_iarray_nat_prod_nat_prod_option
      (if list_all (fun r -> equal_nata (size_list r) xb) xc
        then Some (size_list xc, (xb, IArray (map (fun a -> IArray a) xc)))
        else None);;

let rec mat_of_rows_list_impl
  x1 x2 = rep_isoma (mat_of_rows_list_impl_aux x1 x2);;

type 'a mat = Mat_impl of 'a mat_impl;;

type 'a vec_impl = Abs_vec_impl of (nat * 'a iarray);;

let rec rep_vec_impl (Abs_vec_impl x) = x;;

let rec suba (IArray asa, n) = nth asa (nat_of_integer n);;

let rec sub asa n = suba (asa, integer_of_nat n);;

let rec vec_index_impl xa = (let (_, a) = rep_vec_impl xa in sub a);;

type 'a vec = Vec_impl of 'a vec_impl;;

let rec vec_index (Vec_impl v) i = vec_index_impl v i;;

let rec tabulate
  (n, f) =
    IArray (map (comp f integer_of_nat) (upt zero_nata (nat_of_integer n)));;

let rec of_fun f n = tabulate (integer_of_nat n, comp f nat_of_integer);;

let rec mat_of_fun
  xc xd xe =
    Abs_mat_impl
      (xc, (xd, of_fun (fun i -> of_fun (fun j -> xe (i, j)) xd) xc));;

let rec mat nr nc f = Mat_impl (mat_of_fun nr nc f);;

let rec mat_of_rows
  n rs = mat (size_list rs) n (fun (i, a) -> vec_index (nth rs i) a);;

let rec vec_of_fun xb xc = Abs_vec_impl (xb, of_fun xc xb);;

let rec vec n f = Vec_impl (vec_of_fun n f);;

let rec mat_of_rows_list
  nc vs =
    (match mat_of_rows_list_impl nc vs
      with None -> mat_of_rows nc (map (fun v -> vec nc (nth v)) vs)
      | Some a -> Mat_impl a);;

let rec berlekamp_mat_i _A
  p ff_ops u =
    (let n = degree_i u in
     let ze = zeroa ff_ops in
     let on = onea ff_ops in
     let mul_p =
       power_poly_f_mod_i _A ff_ops (fun v -> mod_field_poly_i _A ff_ops v u)
         [ze; on] (nat p)
       in
     let xks = power_polys_i _A ff_ops mul_p u [on] n in
      mat_of_rows_list n
        (map (fun cs -> cs @ replicate (minus_nata n (size_list cs)) ze) xks));;

let rec eliminate_entries_i2 _A
  xc xe xg xh xi xj =
    Abs_mat_impl
      ((let (nr, (nc, a)) = rep_mat_impl xi in
         (fun i ->
           (nr, (nc, (let ai = suba (a, i) in
                       tabulate
                         (integer_of_nat nr,
                           (fun ia ->
                             (let aia = suba (a, ia) in
                               (if Z.equal ia i then aia
                                 else (let vi_j = xh ia in
(if eq _A vi_j xc then aia
  else tabulate
         (integer_of_nat nc,
           (fun j -> xe (suba (aia, j)) (xg vi_j (suba (ai, j))))))))))))))))
        xj);;

let rec dim_row_impl xa = fst (rep_mat_impl xa);;

let rec eliminate_entries_gen_zero _A
  mm tt z v (Mat_impl m) i j =
    (if less_nat i (dim_row_impl m)
      then Mat_impl (eliminate_entries_i2 _A z mm tt v m (integer_of_nat i))
      else failwith "index out of range in eliminate_entries"
             (fun _ ->
               eliminate_entries_gen_zero _A mm tt z v (Mat_impl m) i j));;

let rec eliminate_entries_i _A
  ops = eliminate_entries_gen_zero _A (minusa ops) (timesa ops) (zeroa ops);;

let rec list_update
  x0 i y = match x0, i, y with [], i, y -> []
    | x :: xs, i, y ->
        (if equal_nata i zero_nata then y :: xs
          else x :: list_update xs (minus_nata i one_nata) y);;

let rec lengtha (IArray asa) = integer_of_nat (size_list asa);;

let rec length asa = nat_of_integer (lengtha asa);;

let rec list_of asa = map (sub asa) (upt zero_nata (length asa));;

let rec mat_swaprows_impl
  xc xd xe =
    Abs_mat_impl
      (let (nr, (nc, a)) = rep_mat_impl xe in
        (if less_nat xc nr && less_nat xd nr
          then (let ai = sub a xc in
                let aj = sub a xd in
                let arows = list_of a in
                let aa = IArray (list_update (list_update arows xc aj) xd ai) in
                 (nr, (nc, aa)))
          else (nr, (nc, a))));;

let rec mat_swaprows
  k l (Mat_impl a) =
    (let nr = dim_row_impl a in
      (if less_nat l nr && less_nat k nr then Mat_impl (mat_swaprows_impl k l a)
        else failwith "index out of bounds in mat_swaprows"
               (fun _ -> mat_swaprows k l (Mat_impl a))));;

let rec mat_multrow_gen_impl
  xc xd xe xf =
    Abs_mat_impl
      (let (nr, (nc, a)) = rep_mat_impl xf in
       let ak = sub a xd in
       let arows = list_of a in
       let aka = IArray (map (xc xe) (list_of ak)) in
       let aa = IArray (list_update arows xd aka) in
        (nr, (nc, aa)));;

let rec mat_multrow_gen
  mul k aa (Mat_impl a) = Mat_impl (mat_multrow_gen_impl mul k aa a);;

let rec multrow_i ops = mat_multrow_gen (timesa ops);;

let rec index_mat_impl
  xa = (let (nr, (_, m)) = rep_mat_impl xa in
         (fun (i, j) ->
           (if less_nat i nr then sub (sub m i) j
             else sub (IArray (nth [] (minus_nata i nr))) j)));;

let rec index_mat (Mat_impl m) ij = index_mat_impl m ij;;

let rec gauss_jordan_main_i _A
  ops nr nc a i j =
    (if less_nat i nr && less_nat j nc
      then (let aij = index_mat a (i, j) in
             (if eq _A aij (zeroa ops)
               then (match
                      maps (fun ia ->
                             (if not (eq _A (index_mat a (ia, j)) (zeroa ops))
                               then [ia] else []))
                        (upt (suc i) nr)
                      with [] -> gauss_jordan_main_i _A ops nr nc a i (suc j)
                      | ia :: _ ->
                        gauss_jordan_main_i _A ops nr nc (mat_swaprows i ia a) i
                          j)
               else (if eq _A aij (onea ops)
                      then (let v =
                              (fun ia -> index_mat a (nat_of_integer ia, j)) in
                             gauss_jordan_main_i _A ops nr nc
                               (eliminate_entries_i _A ops v a i j) (suc i)
                               (suc j))
                      else (let iaij = inversea ops aij in
                            let aa = multrow_i ops i iaij a in
                            let v =
                              (fun ia -> index_mat aa (nat_of_integer ia, j)) in
                             gauss_jordan_main_i _A ops nr nc
                               (eliminate_entries_i _A ops v aa i j) (suc i)
                               (suc j)))))
      else a);;

let rec dim_row (Mat_impl m) = dim_row_impl m;;

let rec dim_col_impl xa = fst (snd (rep_mat_impl xa));;

let rec dim_col (Mat_impl m) = dim_col_impl m;;

let rec gauss_jordan_single_i _A
  ops a =
    gauss_jordan_main_i _A ops (dim_row a) (dim_col a) a zero_nata zero_nata;;

let rec transpose_mat
  a = mat (dim_col a) (dim_row a) (fun (i, j) -> index_mat a (j, i));;

let rec berlekamp_resulting_mat_i _A
  p ff_ops u =
    (let q = berlekamp_mat_i _A p ff_ops u in
     let n = dim_row q in
     let qi =
       mat n n
         (fun (i, j) ->
           (if equal_nata i j
             then minusa ff_ops (index_mat q (i, j)) (onea ff_ops)
             else index_mat q (i, j)))
       in
      gauss_jordan_single_i _A ff_ops (transpose_mat qi));;

let rec pivot_positions_main_gen _A
  zero a nr nc i j =
    (if less_nat i nr
      then (if less_nat j nc
             then (if eq _A (index_mat a (i, j)) zero
                    then pivot_positions_main_gen _A zero a nr nc i (suc j)
                    else (i, j) ::
                           pivot_positions_main_gen _A zero a nr nc (suc i)
                             (suc j))
             else [])
      else []);;

let rec pivot_positions_gen _A
  zer a =
    pivot_positions_main_gen _A zer a (dim_row a) (dim_col a) zero_nata
      zero_nata;;

let rec swap p = (snd p, fst p);;

let rec map_of _A
  x0 k = match x0, k with
    (l, v) :: ps, k -> (if eq _A l k then Some v else map_of _A ps k)
    | [], k -> None;;

let rec non_pivot_base_gen
  uminus zero one a pivots =
    (let _ = dim_row a in
     let nc = dim_col a in
     let invers = map_of equal_nat (map swap pivots) in
      (fun qj ->
        vec nc
          (fun i ->
            (if equal_nata i qj then one
              else (match invers i with None -> zero
                     | Some j -> uminus (index_mat a (j, qj)))))));;

let rec membera _A x0 y = match x0, y with [], y -> false
                     | x :: xs, y -> eq _A x y || membera _A xs y;;

let rec find_base_vectors_gen _A
  uminus zero one a =
    (let pp = pivot_positions_gen _A zero a in
     let b =
       filtera (fun j -> not (membera equal_nat (map snd pp) j))
         (upt zero_nata (dim_col a))
       in
      map (non_pivot_base_gen uminus zero one a pp) b);;

let rec find_base_vectors_i _A
  ops a = find_base_vectors_gen _A (uminusa ops) (zeroa ops) (onea ops) a;;

let rec list_of_vec_impl xa = (let (_, a) = rep_vec_impl xa in list_of a);;

let rec list_of_vec (Vec_impl v) = list_of_vec_impl v;;

let rec berlekamp_basis_i _A
  p ff_ops u =
    map (comp (poly_of_list_i _A ff_ops) list_of_vec)
      (find_base_vectors_i _A ff_ops
        (berlekamp_resulting_mat_i _A p ff_ops u));;

let rec berlekamp_monic_factorization_i _A
  p ff_ops d f =
    (let vs = berlekamp_basis_i _A p ff_ops f in
      berlekamp_factorization_main_i _A p ff_ops (zeroa ff_ops) (onea ff_ops) d
        [f] vs (size_list vs));;

let rec finite_field_factorization_i _A
  p ff_ops f =
    (if equal_nata (degree_i f) zero_nata then (lead_coeff_i ff_ops f, [])
      else (let a = lead_coeff_i ff_ops f in
            let u = smult_i _A ff_ops (inversea ff_ops a) f in
            let gs =
              (if false then distinct_degree_factorization_i _A p ff_ops u
                else [(one_nata, u)])
              in
            let (irr, hs) =
              partition (fun (i, fa) -> equal_nata (degree_i fa) i) gs in
             (a, map snd irr @
                   maps (fun (aa, b) ->
                          berlekamp_monic_factorization_i _A p ff_ops aa b)
                     hs)));;

let rec to_inta
  (Arith_Ops_Record
    (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14))
    = x13;;

let rec to_int_poly_i
  ops f = poly_of_list (comm_monoid_add_int, equal_int) (map (to_inta ops) f);;

let rec of_int_poly_i ops f = map (of_intb ops) (coeffs zero_int f);;

let rec m m x = modulo_inta x m;;

let rec mp ma = map_poly zero_int (zero_int, equal_int) (m ma);;

let rec finite_field_factorization_main _A
  p f_ops f =
    (let (c, fs) =
       finite_field_factorization_i _A p f_ops (of_int_poly_i f_ops (mp p f)) in
      (to_inta f_ops c, map (to_int_poly_i f_ops) fs));;

let rec mult_p_integer p x y = modulo_integer (Z.mul x y) p;;

let rec power_p_integer
  p x n =
    (if Z.leq n Z.zero then (Z.of_int 1)
      else (let reca =
              power_p_integer p (mult_p_integer p x x)
                (Integer_Bit.shiftr n (Z.of_int 1))
              in
             (if Z.equal (Z.logand n (Z.of_int 1)) Z.zero then reca
               else mult_p_integer p reca x)));;

let rec inverse_p_integer
  p x = (if Z.equal x Z.zero then Z.zero
          else power_p_integer p x (Z.sub p (Z.of_int 2)));;

let rec uminus_p_integer
  p x = (if Z.equal x Z.zero then Z.zero else Z.sub p x);;

let rec divide_p_integer p x y = mult_p_integer p x (inverse_p_integer p y);;

let rec minus_p_integer
  p x y = (if Z.leq y x then Z.sub x y else Z.sub (Z.add x p) y);;

let rec plus_p_integer
  p x y = (let z = Z.add x y in (if Z.leq p z then Z.sub z p else z));;

let rec finite_field_ops_integer
  p = Arith_Ops_Record
        (Z.zero, (Z.of_int 1), plus_p_integer p, mult_p_integer p,
          minus_p_integer p, uminus_p_integer p, divide_p_integer p,
          inverse_p_integer p,
          (fun x y -> (if Z.equal y Z.zero then x else Z.zero)),
          (fun x -> (if Z.equal x Z.zero then Z.zero else (Z.of_int 1))),
          (fun x -> x), integer_of_int, (fun a -> Int_of_integer a),
          (fun x -> Z.leq Z.zero x && Z.lt x p));;

let rec push_bit_uint64
  n x = (if less_nat n (nat_of_integer (Z.of_int 64))
          then Uint64.shiftl x (integer_of_nat n) else Int64.zero);;

let mod0_uint64 _ = failwith "Uint64.mod0_uint64";;

let div0_uint64 _ = failwith "Uint64.div0_uint64";;

let rec bit_integer x n = Integer_Bit.test_bit x (integer_of_nat n);;

let rec uint64
  i = (let ia = Z.logand i (Z.of_string "18446744073709551615") in
        (if bit_integer ia (nat_of_integer (Z.of_int 63))
          then Z.to_int64 (Z.sub ia (Z.of_string "18446744073709551616"))
          else Z.to_int64 ia));;

let rec uint64_divmod
  x y = (if Uint64.less_eq (uint64 (Z.of_string "9223372036854775808")) y
          then (if Uint64.less x y then (Int64.zero, x)
                 else (Int64.one, Int64.sub x y))
          else (if (Int64.compare y Int64.zero = 0)
                 then (div0_uint64 x, mod0_uint64 x)
                 else (let q =
                         push_bit_uint64 one_nata
                           (Int64.div (Uint64.shiftr x (Z.of_int 1)) y)
                         in
                       let r = Int64.sub x (Int64.mul q y) in
                        (if Uint64.less_eq y r
                          then (Int64.add q Int64.one, Int64.sub r y)
                          else (q, r)))));;

let rec uint64_mod x y = snd (uint64_divmod x y);;

let rec modulo_uint64
  x y = (if (Int64.compare y Int64.zero = 0) then x else uint64_mod x y);;

let rec mult_p64 p x y = modulo_uint64 (Int64.mul x y) p;;

let rec power_p64
  p x n =
    (if (Int64.compare n Int64.zero = 0) then Int64.one
      else (let reca =
              power_p64 p (mult_p64 p x x) (Uint64.shiftr n (Z.of_int 1)) in
             (if (Int64.compare (Int64.logand n Int64.one) Int64.zero = 0)
               then reca else mult_p64 p reca x)));;

let rec inverse_p64
  p x = (if (Int64.compare x Int64.zero = 0) then Int64.zero
          else power_p64 p x (Int64.sub p (uint64 (Z.of_int 2))));;

let rec uminus_p64
  p x = (if (Int64.compare x Int64.zero = 0) then Int64.zero
          else Int64.sub p x);;

let rec divide_p64 p x y = mult_p64 p x (inverse_p64 p y);;

let rec minus_p64
  p x y =
    (if Uint64.less_eq y x then Int64.sub x y
      else Int64.sub (Int64.add x p) y);;

let rec plus_p64
  p x y =
    (let z = Int64.add x y in
      (if Uint64.less_eq p z then Int64.sub z p else z));;

let rec uint64_of_int i = uint64 (integer_of_int i);;

let rec bit_uint64
  x n = less_nat n (nat_of_integer (Z.of_int 64)) &&
          Uint64.test_bit x (integer_of_nat n);;

let rec integer_of_uint64
  n = (if bit_uint64 n (nat_of_integer (Z.of_int 63))
        then Z.logor
               (Z.of_int64
                 (Int64.logand n (uint64 (Z.of_string "9223372036854775807"))))
               (Z.of_string "9223372036854775808")
        else Z.of_int64 n);;

let rec int_of_uint64 x = Int_of_integer (integer_of_uint64 x);;

let rec finite_field_ops64
  p = Arith_Ops_Record
        (Int64.zero, Int64.one, plus_p64 p, mult_p64 p, minus_p64 p,
          uminus_p64 p, divide_p64 p, inverse_p64 p,
          (fun x y ->
            (if (Int64.compare y Int64.zero = 0) then x else Int64.zero)),
          (fun x ->
            (if (Int64.compare x Int64.zero = 0) then Int64.zero
              else Int64.one)),
          (fun x -> x), uint64_of_int, int_of_uint64,
          (fun x -> Uint64.less_eq Int64.zero x && Uint64.less x p));;

let rec push_bit_uint32
  n x = (if less_nat n (nat_of_integer (Z.of_int 32))
          then Uint32.shiftl x (integer_of_nat n) else Int32.zero);;

let mod0_uint32 _ = failwith "Uint32.mod0_uint32";;

let div0_uint32 _ = failwith "Uint32.div0_uint32";;

let rec uint32
  i = (let ia = Z.logand i (Z.of_string "4294967295") in
        (if bit_integer ia (nat_of_integer (Z.of_int 31))
          then Z.to_int32 (Z.sub ia (Z.of_string "4294967296"))
          else Z.to_int32 ia));;

let rec uint32_divmod
  x y = (if Uint32.less_eq (uint32 (Z.of_string "2147483648")) y
          then (if Uint32.less x y then (Int32.zero, x)
                 else (Int32.one, Int32.sub x y))
          else (if (Int32.compare y Int32.zero = 0)
                 then (div0_uint32 x, mod0_uint32 x)
                 else (let q =
                         push_bit_uint32 one_nata
                           (Int32.div (Uint32.shiftr x (Z.of_int 1)) y)
                         in
                       let r = Int32.sub x (Int32.mul q y) in
                        (if Uint32.less_eq y r
                          then (Int32.add q Int32.one, Int32.sub r y)
                          else (q, r)))));;

let rec uint32_mod x y = snd (uint32_divmod x y);;

let rec modulo_uint32
  x y = (if (Int32.compare y Int32.zero = 0) then x else uint32_mod x y);;

let rec mult_p32 p x y = modulo_uint32 (Int32.mul x y) p;;

let rec power_p32
  p x n =
    (if (Int32.compare n Int32.zero = 0) then Int32.one
      else (let reca =
              power_p32 p (mult_p32 p x x) (Uint32.shiftr n (Z.of_int 1)) in
             (if (Int32.compare (Int32.logand n Int32.one) Int32.zero = 0)
               then reca else mult_p32 p reca x)));;

let rec inverse_p32
  p x = (if (Int32.compare x Int32.zero = 0) then Int32.zero
          else power_p32 p x (Int32.sub p (uint32 (Z.of_int 2))));;

let rec uminus_p32
  p x = (if (Int32.compare x Int32.zero = 0) then Int32.zero
          else Int32.sub p x);;

let rec divide_p32 p x y = mult_p32 p x (inverse_p32 p y);;

let rec minus_p32
  p x y =
    (if Uint32.less_eq y x then Int32.sub x y
      else Int32.sub (Int32.add x p) y);;

let rec plus_p32
  p x y =
    (let z = Int32.add x y in
      (if Uint32.less_eq p z then Int32.sub z p else z));;

let rec uint32_of_int i = uint32 (integer_of_int i);;

let rec bit_uint32
  x n = less_nat n (nat_of_integer (Z.of_int 32)) &&
          Uint32.test_bit x (integer_of_nat n);;

let rec integer_of_uint32
  n = (if bit_uint32 n (nat_of_integer (Z.of_int 31))
        then Z.logor
               (Z.of_int32 (Int32.logand n (uint32 (Z.of_string "2147483647"))))
               (Z.of_string "2147483648")
        else Z.of_int32 n);;

let rec int_of_uint32 x = Int_of_integer (integer_of_uint32 x);;

let rec finite_field_ops32
  p = Arith_Ops_Record
        (Int32.zero, Int32.one, plus_p32 p, mult_p32 p, minus_p32 p,
          uminus_p32 p, divide_p32 p, inverse_p32 p,
          (fun x y ->
            (if (Int32.compare y Int32.zero = 0) then x else Int32.zero)),
          (fun x ->
            (if (Int32.compare x Int32.zero = 0) then Int32.zero
              else Int32.one)),
          (fun x -> x), uint32_of_int, int_of_uint32,
          (fun x -> Uint32.less_eq Int32.zero x && Uint32.less x p));;

let equal_integer = ({equal = Z.equal} : Z.t equal);;

let equal_uint64 =
  ({equal = (fun a b -> (Int64.compare a b = 0))} : int64 equal);;

let equal_uint32 =
  ({equal = (fun a b -> (Int32.compare a b = 0))} : int32 equal);;

let rec finite_field_factorization_int
  p = (if less_eq_int p (Int_of_integer (Z.of_int 65535))
        then finite_field_factorization_main equal_uint32 p
               (finite_field_ops32 (uint32_of_int p))
        else (if less_eq_int p (Int_of_integer (Z.of_string "4294967295"))
               then finite_field_factorization_main equal_uint64 p
                      (finite_field_ops64 (uint64_of_int p))
               else finite_field_factorization_main equal_integer p
                      (finite_field_ops_integer (integer_of_int p))));;

type ('a, 'b, 'c) subseqs_foldr_impl =
  Sublists_Foldr_Impl of
    ('b -> 'a list -> nat -> 'b list * 'c) * ('c -> 'b list * 'c);;

let rec subseqs_foldr (Sublists_Foldr_Impl (x1, x2)) = x1;;

let rec next_subseqs_foldr (Sublists_Foldr_Impl (x1, x2)) = x2;;

let rec divide_poly_main_list (_A1, _A2)
  lc q r d n =
    (if equal_nata n zero_nata then q
      else (let cr = hda r in
             (if eq _A1 cr
                   (zero _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
               then divide_poly_main_list (_A1, _A2) lc
                      (cCons
                        (_A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
                          _A1)
                        cr q)
                      (tla r) d (minus_nata n one_nata)
               else (let a =
                       divide
                         _A2.semidom_divide_idom_divide.divide_semidom_divide cr
                         lc
                       in
                     let qq =
                       cCons (_A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
                               _A1)
                         a q
                       in
                     let rr =
                       minus_poly_rev_list
                         _A2.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.group_add_neg_numeral
                         r (map (times
                                  _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd
                                  a)
                             d)
                       in
                      (if eq _A1 (hda rr)
                            (zero _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
                        then divide_poly_main_list (_A1, _A2) lc qq (tla rr) d
                               (minus_nata n one_nata)
                        else [])))));;

let rec divide_poly_list (_A1, _A2)
  f g = (let cg =
           coeffs
             _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
             g
           in
          (if null cg then g
            else (let cf =
                    coeffs
                      _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
                      f
                    in
                  let cgr = rev cg in
                   poly_of_list
                     (_A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.comm_monoid_add_semiring_0,
                       _A1)
                     (divide_poly_main_list (_A1, _A2) (hda cgr) [] (rev cf) cgr
                       (minus_nata (plus_nata one_nata (size_list cf))
                         (size_list cg))))));;

let rec divide_polya (_A1, _A2) f g = divide_poly_list (_A1, _A2) f g;;

let rec plus_coeffs (_A1, _A2)
  xs x1 = match xs, x1 with xs, [] -> xs
    | [], v :: va -> v :: va
    | x :: xs, y :: ys ->
        cCons (_A1.monoid_add_comm_monoid_add.zero_monoid_add, _A2)
          (plus _A1.monoid_add_comm_monoid_add.semigroup_add_monoid_add.plus_semigroup_add
            x y)
          (plus_coeffs (_A1, _A2) xs ys);;

let rec plus_polya (_A1, _A2)
  p q = Poly (plus_coeffs (_A1, _A2)
               (coeffs _A1.monoid_add_comm_monoid_add.zero_monoid_add p)
               (coeffs _A1.monoid_add_comm_monoid_add.zero_monoid_add q));;

let rec minus_polya (_A1, _A2)
  p q = plus_polya
          (_A1.cancel_comm_monoid_add_ab_group_add.comm_monoid_add_cancel_comm_monoid_add,
            _A2)
          p (uminus_polya _A1 q);;

let rec coeffs_minus _A
  xs x1 = match xs, x1 with
    x :: xs, y :: ys ->
      minus _A.group_add_ab_group_add.minus_group_add x y ::
        coeffs_minus _A xs ys
    | xs, [] -> xs
    | [], v :: va ->
        map (uminus _A.group_add_ab_group_add.uminus_group_add) (v :: va);;

let rec monom_mult _A
  n f = Poly (let xs =
                coeffs
                  _A.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
                  f
                in
               (if null xs then xs
                 else replicate n
                        (zero _A.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero) @
                        xs));;

let rec smult (_A1, _A2, _A3)
  a p = Poly (if eq _A1 a
                   (zero _A2.semiring_0_comm_semiring_0.mult_zero_semiring_0.zero_mult_zero)
               then []
               else map (times
                          _A2.semiring_0_comm_semiring_0.mult_zero_semiring_0.times_mult_zero
                          a)
                      (coeffs
                        _A2.semiring_0_comm_semiring_0.mult_zero_semiring_0.zero_mult_zero
                        p));;

let rec karatsuba_main (_A1, _A2, _A3)
  f n g m =
    (if less_eq_nat n karatsuba_lower_bound ||
          less_eq_nat m karatsuba_lower_bound
      then (let ff =
              poly_of_list
                (_A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.comm_monoid_add_semiring_0,
                  _A1)
                f
              in
             foldr (fun a p ->
                     plus_polya
                       (_A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.comm_monoid_add_semiring_0,
                         _A1)
                       (smult
                         (_A1, _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.comm_semiring_0_comm_semiring_1,
                           _A3)
                         a ff)
                       (pCons
                         (_A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
                           _A1)
                         (zero _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
                         p))
               g (zero_polya
                   _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero))
      else (let n2 = divide_nata n (nat_of_integer (Z.of_int 2)) in
             (if less_nat n2 m
               then (let (f0, f1) = split_at n2 f in
                     let (g0, g1) = split_at n2 g in
                     let p1 =
                       karatsuba_main (_A1, _A2, _A3) f1 (minus_nata n n2) g1
                         (minus_nata m n2)
                       in
                     let p2 =
                       karatsuba_main (_A1, _A2, _A3)
                         (coeffs_minus
                           _A2.ring_1_comm_ring_1.ring_ring_1.ab_group_add_ring
                           f1 f0)
                         n2 (coeffs_minus
                              _A2.ring_1_comm_ring_1.ring_ring_1.ab_group_add_ring
                              g1 g0)
                         n2
                       in
                     let p3 = karatsuba_main (_A1, _A2, _A3) f0 n2 g0 n2 in
                      plus_polya
                        (_A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.comm_monoid_add_semiring_0,
                          _A1)
                        (monom_mult
                          _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel
                          (plus_nata n2 n2) p1)
                        (plus_polya
                          (_A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.comm_monoid_add_semiring_0,
                            _A1)
                          (monom_mult
                            _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel
                            n2 (plus_polya
                                 (_A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.comm_monoid_add_semiring_0,
                                   _A1)
                                 (minus_polya
                                   (_A2.ring_1_comm_ring_1.ring_ring_1.ab_group_add_ring,
                                     _A1)
                                   p1 p2)
                                 p3))
                          p3))
               else (let (f0, f1) = split_at n2 f in
                     let p1 =
                       karatsuba_main (_A1, _A2, _A3) f1 (minus_nata n n2) g m
                       in
                     let a = karatsuba_main (_A1, _A2, _A3) f0 n2 g m in
                      plus_polya
                        (_A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.comm_monoid_add_semiring_0,
                          _A1)
                        (monom_mult
                          _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel
                          n2 p1)
                        a))));;

let rec karatsuba_mult_poly (_A1, _A2, _A3)
  f g = (let ff =
           coeffs
             _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
             f
           in
         let gg =
           coeffs
             _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
             g
           in
         let n = size_list ff in
         let m = size_list gg in
          (if less_eq_nat n karatsuba_lower_bound ||
                less_eq_nat m karatsuba_lower_bound
            then (if less_eq_nat n m
                   then foldr (fun a p ->
                                plus_polya
                                  (_A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.comm_monoid_add_semiring_0,
                                    _A1)
                                  (smult
                                    (_A1, _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.comm_semiring_0_comm_semiring_1,
                                      _A3)
                                    a g)
                                  (pCons
                                    (_A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
                                      _A1)
                                    (zero _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
                                    p))
                          ff (zero_polya
                               _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
                   else foldr (fun a p ->
                                plus_polya
                                  (_A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.comm_monoid_add_semiring_0,
                                    _A1)
                                  (smult
                                    (_A1, _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.comm_semiring_0_comm_semiring_1,
                                      _A3)
                                    a f)
                                  (pCons
                                    (_A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
                                      _A1)
                                    (zero _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
                                    p))
                          gg (zero_polya
                               _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero))
            else (if less_eq_nat n m
                   then karatsuba_main (_A1, _A2, _A3) gg m ff n
                   else karatsuba_main (_A1, _A2, _A3) ff n gg m)));;

let rec one_polya _A
  = Poly [one _A.semiring_1_comm_semiring_1.semiring_numeral_semiring_1.numeral_semiring_numeral.one_numeral];;

let rec prod_list_m
  m x1 = match m, x1 with m, [] -> one_polya comm_semiring_1_int
    | m, f :: fs ->
        mp m (karatsuba_mult_poly
               (equal_int, comm_ring_1_int, semiring_no_zero_divisors_int) f
               (prod_list_m m fs));;

let rec inv_M2
  m m2 = (fun x -> (if less_eq_int x m2 then x else minus_inta x m));;

let rec primitive_part (_A1, _A2)
  p = map_poly _A1.comm_monoid_gcd_semiring_gcd.gcd_comm_monoid_gcd.zero_gcd
        (_A1.comm_monoid_gcd_semiring_gcd.gcd_comm_monoid_gcd.zero_gcd, _A2)
        (fun x ->
          divide
            _A1.normalization_semidom_semiring_gcd.algebraic_semidom_normalization_semidom.semidom_divide_algebraic_semidom.divide_semidom_divide
            x (content _A1 p))
        p;;

let rec divmod_int
  m n = map_prod (fun a -> Int_of_integer a) (fun a -> Int_of_integer a)
          (divmod_integer (integer_of_int m) (integer_of_int n));;

let rec div_mod_int_poly
  p q = (if equal_polya (zero_int, equal_int) q (zero_polya zero_int) then None
          else (let n = degreea zero_int q in
                let _ = coeffa zero_int q n in
                 fold_coeffs zero_int
                   (fun a b ->
                     (match b with None -> None
                       | Some (s, r) ->
                         (let ar = pCons (zero_int, equal_int) a r in
                          let (ba, m) =
                            divmod_int (coeffa zero_int ar (degreea zero_int q))
                              (coeffa zero_int q (degreea zero_int q))
                            in
                           (if equal_inta m zero_inta
                             then Some (pCons (zero_int, equal_int) ba s,
 minus_polya (ab_group_add_int, equal_int) ar
   (smult (equal_int, comm_semiring_0_int, semiring_no_zero_divisors_int) ba q))
                             else None))))
                   p (Some (zero_polya zero_int, zero_polya zero_int))));;

let rec div_int_poly
  p q = (match div_mod_int_poly p q with None -> None
          | Some (d, m) ->
            (if equal_polya (zero_int, equal_int) m (zero_polya zero_int)
              then Some d else None));;

let rec dvd_int_poly
  q p = (if equal_polya (zero_int, equal_int) q (zero_polya zero_int)
          then equal_polya (zero_int, equal_int) p (zero_polya zero_int)
          else not (is_none (div_int_poly p q)));;

let rec remove1 _A
  x xa1 = match x, xa1 with x, [] -> []
    | x, y :: xs -> (if eq _A x y then xs else y :: remove1 _A x xs);;

let rec equal_poly (_A1, _A2) =
  ({equal = equal_polya (_A1, _A2)} : 'a poly equal);;

let rec reconstruction
  m sl_impl m2 state u luu lu d r vs res cands =
    (match cands
      with [] ->
        (let da = suc d in
          (if less_nat r (plus_nata da da) then u :: res
            else (let (candsa, statea) = next_subseqs_foldr sl_impl state in
                   reconstruction m sl_impl m2 statea u luu lu da r vs res
                     candsa)))
      | (lv, ws) :: candsa ->
        (let lva = inv_M2 m m2 lv in
          (if dvd (equal_int, semidom_modulo_int) lva
                (match coeffs zero_int luu with [] -> zero_inta | x :: _ -> x)
            then (let vb =
                    map_poly zero_int (zero_int, equal_int) (inv_M2 m m2)
                      (mp m (smult
                              (equal_int, comm_semiring_0_int,
                                semiring_no_zero_divisors_int)
                              lu (prod_list_m m ws)))
                    in
                   (if dvd_int_poly vb luu
                     then (let pp_vb =
                             primitive_part (semiring_gcd_int, equal_int) vb in
                           let ua =
                             divide_polya (equal_int, idom_divide_int) u pp_vb
                             in
                           let ra = minus_nata r (size_list ws) in
                           let resa = pp_vb :: res in
                            (if less_nat ra (plus_nata d d) then ua :: resa
                              else (let lua =
                                      coeffa zero_int ua (degreea zero_int ua)
                                      in
                                    let vsa =
                                      fold
(remove1 (equal_poly (zero_int, equal_int))) ws vs
                                      in
                                    let (candsb, statea) =
                                      subseqs_foldr sl_impl (lua, []) vsa d in
                                     reconstruction m sl_impl m2 statea ua
                                       (smult
 (equal_int, comm_semiring_0_int, semiring_no_zero_divisors_int) lua ua)
                                       lua d ra vsa resa candsb)))
                     else reconstruction m sl_impl m2 state u luu lu d r vs res
                            candsa))
            else reconstruction m sl_impl m2 state u luu lu d r vs res
                   candsa)));;

let rec zassenhaus_reconstruction_generic
  sl_impl vs p n f =
    (let lf = coeffa zero_int f (degreea zero_int f) in
     let pn = binary_power monoid_mult_int p n in
     let (_, state) = subseqs_foldr sl_impl (lf, []) vs zero_nata in
      reconstruction pn sl_impl (divide_inta pn (Int_of_integer (Z.of_int 2)))
        state f
        (smult (equal_int, comm_semiring_0_int, semiring_no_zero_divisors_int)
          lf f)
        lf zero_nata (size_list vs) vs [] []);;

let rec next_subseqs1
  f head tail ret0 ret1 x5 = match f, head, tail, ret0, ret1, x5 with
    f, head, tail, ret0, ret1, (i, v) :: prevs ->
      next_subseqs2 f head tail (f head v :: ret0) ret1 prevs v
        (upt zero_nata i)
    | f, head, tail, ret0, ret1, [] -> (ret0, (head, (tail, ret1)))
and next_subseqs2
  f head tail ret0 ret1 prevs v x7 = match
    f, head, tail, ret0, ret1, prevs, v, x7 with
    f, head, tail, ret0, ret1, prevs, v, j :: js ->
      (let va = f (sub tail j) v in
        next_subseqs2 f head tail (va :: ret0) ((j, va) :: ret1) prevs v js)
    | f, head, tail, ret0, ret1, prevs, v, [] ->
        next_subseqs1 f head tail ret0 ret1 prevs;;

let rec next_subseqs
  f (head, (tail, prevs)) = next_subseqs1 f head tail [] [] prevs;;

let rec create_subseqs
  f base elements n =
    (if equal_nata n zero_nata
      then (if null elements
             then ([base], (failwith "undefined", (IArray [], [])))
             else (let head = hda elements in
                   let tail = IArray (tla elements) in
                    ([base], (head, (tail, [(length tail, base)])))))
      else next_subseqs f
             (snd (create_subseqs f base elements (minus_nata n one_nata))));;

let rec impl f = Sublists_Foldr_Impl (create_subseqs f, next_subseqs f);;

let rec mul_const
  m p c =
    modulo_inta
      (times_inta (match coeffs zero_int p with [] -> zero_inta | x :: _ -> x)
        c)
      m;;

let rec zassenhaus_reconstruction
  vs p n f =
    (let mul = mul_const (binary_power monoid_mult_int p n) in
     let sl_impl = impl (fun x -> map_prod (mul x) (fun a -> x :: a)) in
      zassenhaus_reconstruction_generic sl_impl vs p n f);;

let rec find_exponent_main
  p pm m bnd =
    (if less_int bnd pm then m
      else find_exponent_main p (times_inta pm p) (suc m) bnd);;

let rec find_exponent p bnd = find_exponent_main p p one_nata bnd;;

let rec coprime (_A1, _A2)
  a b = eq _A2 (gcda _A1.comm_monoid_gcd_semiring_gcd.gcd_comm_monoid_gcd a b)
          (one _A1.comm_monoid_gcd_semiring_gcd.gcd_comm_monoid_gcd.one_gcd);;

let rec coprimea (_A1, _A2) = coprime (_A1, _A2);;

let rec pderiv_main_i _A
  ops f x2 = match ops, f, x2 with
    ops, f, x :: xs ->
      cCons_i _A ops (timesa ops f x)
        (pderiv_main_i _A ops (plusa ops f (onea ops)) xs)
    | ops, f, [] -> [];;

let rec pderiv_i _A ops xs = pderiv_main_i _A ops (onea ops) (tla xs);;

let rec separable_i _A
  ops xs =
    equal_lista _A (gcd_poly_i _A ops xs (pderiv_i _A ops xs)) [onea ops];;

let rec separable_impl_main _A
  p ff_ops f = separable_i _A ff_ops (of_int_poly_i ff_ops (mp p f));;

let rec separable_impl
  p = (if less_eq_int p (Int_of_integer (Z.of_int 65535))
        then separable_impl_main equal_uint32 p
               (finite_field_ops32 (uint32_of_int p))
        else (if less_eq_int p (Int_of_integer (Z.of_string "4294967295"))
               then separable_impl_main equal_uint64 p
                      (finite_field_ops64 (uint64_of_int p))
               else separable_impl_main equal_integer p
                      (finite_field_ops_integer (integer_of_int p))));;

let primes_1000 : nat list
  = [nat_of_integer (Z.of_int 2); nat_of_integer (Z.of_int 3);
      nat_of_integer (Z.of_int 5); nat_of_integer (Z.of_int 7);
      nat_of_integer (Z.of_int 11); nat_of_integer (Z.of_int 13);
      nat_of_integer (Z.of_int 17); nat_of_integer (Z.of_int 19);
      nat_of_integer (Z.of_int 23); nat_of_integer (Z.of_int 29);
      nat_of_integer (Z.of_int 31); nat_of_integer (Z.of_int 37);
      nat_of_integer (Z.of_int 41); nat_of_integer (Z.of_int 43);
      nat_of_integer (Z.of_int 47); nat_of_integer (Z.of_int 53);
      nat_of_integer (Z.of_int 59); nat_of_integer (Z.of_int 61);
      nat_of_integer (Z.of_int 67); nat_of_integer (Z.of_int 71);
      nat_of_integer (Z.of_int 73); nat_of_integer (Z.of_int 79);
      nat_of_integer (Z.of_int 83); nat_of_integer (Z.of_int 89);
      nat_of_integer (Z.of_int 97); nat_of_integer (Z.of_int 101);
      nat_of_integer (Z.of_int 103); nat_of_integer (Z.of_int 107);
      nat_of_integer (Z.of_int 109); nat_of_integer (Z.of_int 113);
      nat_of_integer (Z.of_int 127); nat_of_integer (Z.of_int 131);
      nat_of_integer (Z.of_int 137); nat_of_integer (Z.of_int 139);
      nat_of_integer (Z.of_int 149); nat_of_integer (Z.of_int 151);
      nat_of_integer (Z.of_int 157); nat_of_integer (Z.of_int 163);
      nat_of_integer (Z.of_int 167); nat_of_integer (Z.of_int 173);
      nat_of_integer (Z.of_int 179); nat_of_integer (Z.of_int 181);
      nat_of_integer (Z.of_int 191); nat_of_integer (Z.of_int 193);
      nat_of_integer (Z.of_int 197); nat_of_integer (Z.of_int 199);
      nat_of_integer (Z.of_int 211); nat_of_integer (Z.of_int 223);
      nat_of_integer (Z.of_int 227); nat_of_integer (Z.of_int 229);
      nat_of_integer (Z.of_int 233); nat_of_integer (Z.of_int 239);
      nat_of_integer (Z.of_int 241); nat_of_integer (Z.of_int 251);
      nat_of_integer (Z.of_int 257); nat_of_integer (Z.of_int 263);
      nat_of_integer (Z.of_int 269); nat_of_integer (Z.of_int 271);
      nat_of_integer (Z.of_int 277); nat_of_integer (Z.of_int 281);
      nat_of_integer (Z.of_int 283); nat_of_integer (Z.of_int 293);
      nat_of_integer (Z.of_int 307); nat_of_integer (Z.of_int 311);
      nat_of_integer (Z.of_int 313); nat_of_integer (Z.of_int 317);
      nat_of_integer (Z.of_int 331); nat_of_integer (Z.of_int 337);
      nat_of_integer (Z.of_int 347); nat_of_integer (Z.of_int 349);
      nat_of_integer (Z.of_int 353); nat_of_integer (Z.of_int 359);
      nat_of_integer (Z.of_int 367); nat_of_integer (Z.of_int 373);
      nat_of_integer (Z.of_int 379); nat_of_integer (Z.of_int 383);
      nat_of_integer (Z.of_int 389); nat_of_integer (Z.of_int 397);
      nat_of_integer (Z.of_int 401); nat_of_integer (Z.of_int 409);
      nat_of_integer (Z.of_int 419); nat_of_integer (Z.of_int 421);
      nat_of_integer (Z.of_int 431); nat_of_integer (Z.of_int 433);
      nat_of_integer (Z.of_int 439); nat_of_integer (Z.of_int 443);
      nat_of_integer (Z.of_int 449); nat_of_integer (Z.of_int 457);
      nat_of_integer (Z.of_int 461); nat_of_integer (Z.of_int 463);
      nat_of_integer (Z.of_int 467); nat_of_integer (Z.of_int 479);
      nat_of_integer (Z.of_int 487); nat_of_integer (Z.of_int 491);
      nat_of_integer (Z.of_int 499); nat_of_integer (Z.of_int 503);
      nat_of_integer (Z.of_int 509); nat_of_integer (Z.of_int 521);
      nat_of_integer (Z.of_int 523); nat_of_integer (Z.of_int 541);
      nat_of_integer (Z.of_int 547); nat_of_integer (Z.of_int 557);
      nat_of_integer (Z.of_int 563); nat_of_integer (Z.of_int 569);
      nat_of_integer (Z.of_int 571); nat_of_integer (Z.of_int 577);
      nat_of_integer (Z.of_int 587); nat_of_integer (Z.of_int 593);
      nat_of_integer (Z.of_int 599); nat_of_integer (Z.of_int 601);
      nat_of_integer (Z.of_int 607); nat_of_integer (Z.of_int 613);
      nat_of_integer (Z.of_int 617); nat_of_integer (Z.of_int 619);
      nat_of_integer (Z.of_int 631); nat_of_integer (Z.of_int 641);
      nat_of_integer (Z.of_int 643); nat_of_integer (Z.of_int 647);
      nat_of_integer (Z.of_int 653); nat_of_integer (Z.of_int 659);
      nat_of_integer (Z.of_int 661); nat_of_integer (Z.of_int 673);
      nat_of_integer (Z.of_int 677); nat_of_integer (Z.of_int 683);
      nat_of_integer (Z.of_int 691); nat_of_integer (Z.of_int 701);
      nat_of_integer (Z.of_int 709); nat_of_integer (Z.of_int 719);
      nat_of_integer (Z.of_int 727); nat_of_integer (Z.of_int 733);
      nat_of_integer (Z.of_int 739); nat_of_integer (Z.of_int 743);
      nat_of_integer (Z.of_int 751); nat_of_integer (Z.of_int 757);
      nat_of_integer (Z.of_int 761); nat_of_integer (Z.of_int 769);
      nat_of_integer (Z.of_int 773); nat_of_integer (Z.of_int 787);
      nat_of_integer (Z.of_int 797); nat_of_integer (Z.of_int 809);
      nat_of_integer (Z.of_int 811); nat_of_integer (Z.of_int 821);
      nat_of_integer (Z.of_int 823); nat_of_integer (Z.of_int 827);
      nat_of_integer (Z.of_int 829); nat_of_integer (Z.of_int 839);
      nat_of_integer (Z.of_int 853); nat_of_integer (Z.of_int 857);
      nat_of_integer (Z.of_int 859); nat_of_integer (Z.of_int 863);
      nat_of_integer (Z.of_int 877); nat_of_integer (Z.of_int 881);
      nat_of_integer (Z.of_int 883); nat_of_integer (Z.of_int 887);
      nat_of_integer (Z.of_int 907); nat_of_integer (Z.of_int 911);
      nat_of_integer (Z.of_int 919); nat_of_integer (Z.of_int 929);
      nat_of_integer (Z.of_int 937); nat_of_integer (Z.of_int 941);
      nat_of_integer (Z.of_int 947); nat_of_integer (Z.of_int 953);
      nat_of_integer (Z.of_int 967); nat_of_integer (Z.of_int 971);
      nat_of_integer (Z.of_int 977); nat_of_integer (Z.of_int 983);
      nat_of_integer (Z.of_int 991); nat_of_integer (Z.of_int 997)];;

let rec next_candidates
  n = (if equal_nata n zero_nata
        then (nat_of_integer (Z.of_int 1001), primes_1000)
        else (plus_nata n (nat_of_integer (Z.of_int 30)),
               [n; plus_nata n (nat_of_integer (Z.of_int 2));
                 plus_nata n (nat_of_integer (Z.of_int 6));
                 plus_nata n (nat_of_integer (Z.of_int 8));
                 plus_nata n (nat_of_integer (Z.of_int 12));
                 plus_nata n (nat_of_integer (Z.of_int 18));
                 plus_nata n (nat_of_integer (Z.of_int 20));
                 plus_nata n (nat_of_integer (Z.of_int 26))]));;

let rec all_interval_nat
  p i j = less_eq_nat j i || p i && all_interval_nat p (suc i) j;;

let rec prime_nat
  p = less_nat one_nata p &&
        all_interval_nat
          (fun n -> not (dvd (equal_nat, semidom_modulo_nat) n p))
          (suc one_nata) p;;

let rec next_primes
  n = (if equal_nata n zero_nata then next_candidates zero_nata
        else (let (m, ps) = next_candidates n in (m, filtera prime_nat ps)));;

let rec find_prime_main
  f np ps =
    (match ps with [] -> (let (a, b) = next_primes np in find_prime_main f a b)
      | p :: psa -> (if f p then p else find_prime_main f np psa));;

let rec find_prime f = find_prime_main f zero_nata [];;

let rec suitable_prime_bz
  f = (let lc = coeffa zero_int f (degreea zero_int f) in
        int_of_nat
          (find_prime
            (fun n ->
              (let p = int_of_nat n in
                coprimea (semiring_gcd_int, equal_int) lc p &&
                  separable_impl p f))));;

type 'a factor_tree = Factor_Leaf of 'a * int poly |
  Factor_Node of 'a * 'a factor_tree * 'a factor_tree;;

let rec factor_node_info = function Factor_Leaf (i, x) -> i
                           | Factor_Node (i, l, r) -> i;;

let rec product_factor_tree
  p x1 = match p, x1 with p, Factor_Leaf (i, x) -> Factor_Leaf (x, x)
    | p, Factor_Node (i, l, r) ->
        (let la = product_factor_tree p l in
         let ra = product_factor_tree p r in
         let f = factor_node_info la in
         let g = factor_node_info ra in
         let fg =
           mp p (karatsuba_mult_poly
                  (equal_int, comm_ring_1_int, semiring_no_zero_divisors_int) f
                  g)
           in
          Factor_Node (fg, la, ra));;

let rec dividea
  (Arith_Ops_Record
    (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14))
    = x7;;

let rec euclid_ext_aux_i _A
  ops sa s ta t ra r =
    (if eq _A r (zeroa ops)
      then (let c = dividea ops (onea ops) (unit_factora ops ra) in
             ((timesa ops sa c, timesa ops ta c), normalizeb ops ra))
      else (let q = dividea ops ra r in
             euclid_ext_aux_i _A ops s (minusa ops sa (timesa ops q s)) t
               (minusa ops ta (timesa ops q t)) r (moduloa ops ra r)));;

let rec euclid_ext_poly_i _A
  ops = euclid_ext_aux_i (equal_list _A) (poly_ops _A ops)
          (onea (poly_ops _A ops)) (zeroa (poly_ops _A ops))
          (zeroa (poly_ops _A ops)) (onea (poly_ops _A ops));;

let rec bezout_coefficients_i _A
  ff_ops f g = fst (euclid_ext_poly_i _A ff_ops f g);;

let rec euclid_ext_poly_mod_main _A
  p ff_ops f g =
    (let (a, b) =
       bezout_coefficients_i _A ff_ops (of_int_poly_i ff_ops f)
         (of_int_poly_i ff_ops g)
       in
      (to_int_poly_i ff_ops a, to_int_poly_i ff_ops b));;

let rec euclid_ext_poly_dynamic
  p = (if less_eq_int p (Int_of_integer (Z.of_int 65535))
        then euclid_ext_poly_mod_main equal_uint32 p
               (finite_field_ops32 (uint32_of_int p))
        else (if less_eq_int p (Int_of_integer (Z.of_string "4294967295"))
               then euclid_ext_poly_mod_main equal_uint64 p
                      (finite_field_ops64 (uint64_of_int p))
               else euclid_ext_poly_mod_main equal_integer p
                      (finite_field_ops_integer (integer_of_int p))));;

let rec pdivmod_monic_i _A
  ops cf cg =
    (let (q, r) =
       divmod_poly_one_main_i _A ops [] (rev cf) (rev cg)
         (minus_nata (plus_nata one_nata (size_list cf)) (size_list cg))
       in
      (poly_of_list_i _A ops q, poly_of_list_i _A ops (rev r)));;

let rec dupe_monic_i _A
  ops d h s t u =
    (let (q, a) = pdivmod_monic_i _A ops (times_poly_i _A ops t u) d in
      (plus_poly_i _A ops (times_poly_i _A ops s u) (times_poly_i _A ops h q),
        a));;

let rec dupe_monic_i_int _A
  ops d h s t =
    (let da = of_int_poly_i ops d in
     let ha = of_int_poly_i ops h in
     let sa = of_int_poly_i ops s in
     let ta = of_int_poly_i ops t in
      (fun u ->
        (let (db, hb) = dupe_monic_i _A ops da ha sa ta (of_int_poly_i ops u) in
          (to_int_poly_i ops db, to_int_poly_i ops hb))));;

let rec dupe_monic_dynamic
  p = (if less_eq_int p (Int_of_integer (Z.of_int 65535))
        then dupe_monic_i_int equal_uint32
               (finite_field_ops32 (uint32_of_int p))
        else (if less_eq_int p (Int_of_integer (Z.of_string "4294967295"))
               then dupe_monic_i_int equal_uint64
                      (finite_field_ops64 (uint64_of_int p))
               else dupe_monic_i_int equal_integer
                      (finite_field_ops_integer (integer_of_int p))));;

let rec simple_quadratic_hensel_step
  c q s t d h =
    (let u =
       mp q (sdiv_poly (equal_int, idom_divide_int)
              (minus_polya (ab_group_add_int, equal_int) c
                (karatsuba_mult_poly
                  (equal_int, comm_ring_1_int, semiring_no_zero_divisors_int) d
                  h))
              q)
       in
     let (a, b) = dupe_monic_dynamic q d h s t u in
     let da =
       plus_polya (comm_monoid_add_int, equal_int) d
         (smult (equal_int, comm_semiring_0_int, semiring_no_zero_divisors_int)
           q b)
       in
     let aa =
       plus_polya (comm_monoid_add_int, equal_int) h
         (smult (equal_int, comm_semiring_0_int, semiring_no_zero_divisors_int)
           q a)
       in
      (da, aa));;

let rec quadratic_hensel_step
  c q s t d h =
    (let dupe = dupe_monic_dynamic q d h s t in
     let u =
       mp q (sdiv_poly (equal_int, idom_divide_int)
              (minus_polya (ab_group_add_int, equal_int) c
                (karatsuba_mult_poly
                  (equal_int, comm_ring_1_int, semiring_no_zero_divisors_int) d
                  h))
              q)
       in
     let (a, b) = dupe u in
     let da =
       plus_polya (comm_monoid_add_int, equal_int) d
         (smult (equal_int, comm_semiring_0_int, semiring_no_zero_divisors_int)
           q b)
       in
     let ha =
       plus_polya (comm_monoid_add_int, equal_int) h
         (smult (equal_int, comm_semiring_0_int, semiring_no_zero_divisors_int)
           q a)
       in
     let ua =
       mp q (sdiv_poly (equal_int, idom_divide_int)
              (minus_polya (ab_group_add_int, equal_int)
                (plus_polya (comm_monoid_add_int, equal_int)
                  (karatsuba_mult_poly
                    (equal_int, comm_ring_1_int, semiring_no_zero_divisors_int)
                    s da)
                  (karatsuba_mult_poly
                    (equal_int, comm_ring_1_int, semiring_no_zero_divisors_int)
                    t ha))
                (one_polya comm_semiring_1_int))
              q)
       in
     let (aa, ba) = dupe ua in
     let qa = times_inta q q in
     let sa =
       mp qa (minus_polya (ab_group_add_int, equal_int) s
               (smult
                 (equal_int, comm_semiring_0_int, semiring_no_zero_divisors_int)
                 q aa))
       in
     let ta =
       mp qa (minus_polya (ab_group_add_int, equal_int) t
               (smult
                 (equal_int, comm_semiring_0_int, semiring_no_zero_divisors_int)
                 q ba))
       in
      (sa, (ta, (da, ha))));;

let rec quadratic_hensel_loop
  c p s1 t1 d1 h1 j =
    (if less_eq_nat j one_nata then (p, (s1, (t1, (d1, h1))))
      else (if dvd (equal_nat, semidom_modulo_nat) (nat_of_integer (Z.of_int 2))
                 j
             then (let (q, (s, (t, (d, h)))) =
                     quadratic_hensel_loop c p s1 t1 d1 h1
                       (divide_nata j (nat_of_integer (Z.of_int 2)))
                     in
                   let qq = times_inta q q in
                   let (sa, (ta, (da, ha))) = quadratic_hensel_step c q s t d h
                     in
                    (qq, (sa, (ta, (da, ha)))))
             else (let (q, (s, (t, (d, h)))) =
                     quadratic_hensel_loop c p s1 t1 d1 h1
                       (plus_nata (divide_nata j (nat_of_integer (Z.of_int 2)))
                         one_nata)
                     in
                   let (sa, (ta, (da, ha))) = quadratic_hensel_step c q s t d h
                     in
                   let qq = times_inta q q in
                   let pj = divide_inta qq p in
                   let down = mp pj in
                    (pj, (down sa, (down ta, (down da, down ha)))))));;

let rec quadratic_hensel_main
  c p s1 t1 d1 h1 j =
    (if less_eq_nat j one_nata then (d1, h1)
      else (if dvd (equal_nat, semidom_modulo_nat) (nat_of_integer (Z.of_int 2))
                 j
             then (let (q, (s, (t, (a, b)))) =
                     quadratic_hensel_loop c p s1 t1 d1 h1
                       (divide_nata j (nat_of_integer (Z.of_int 2)))
                     in
                    simple_quadratic_hensel_step c q s t a b)
             else (let (q, (s, (t, (d, h)))) =
                     quadratic_hensel_loop c p s1 t1 d1 h1
                       (plus_nata (divide_nata j (nat_of_integer (Z.of_int 2)))
                         one_nata)
                     in
                   let (da, ha) = simple_quadratic_hensel_step c q s t d h in
                   let down = mp (divide_inta (times_inta q q) p) in
                    (down da, down ha))));;

let rec quadratic_hensel_binary
  p n c d h =
    (let (s, t) = euclid_ext_poly_dynamic p d h in
      quadratic_hensel_main c p s t d h n);;

let rec hensel_lifting_main
  p n u x3 = match p, n, u, x3 with p, n, u, Factor_Leaf (uu, uv) -> [u]
    | p, n, u, Factor_Node (uw, l, r) ->
        (let v = factor_node_info l in
         let w = factor_node_info r in
         let (va, wa) = quadratic_hensel_binary p n u v w in
          hensel_lifting_main p n va l @ hensel_lifting_main p n wa r);;

let rec partition_factors_main
  s x1 = match s, x1 with s, [] -> ([], [])
    | s, (f, d) :: xs ->
        (if less_eq_nat d s
          then (let (l, a) = partition_factors_main (minus_nata s d) xs in
                 ((f, d) :: l, a))
          else (let (l, r) = partition_factors_main d xs in (l, (f, d) :: r)));;

let rec sum_list _A
  xs = foldr (plus _A.semigroup_add_monoid_add.plus_semigroup_add) xs
         (zero _A.zero_monoid_add);;

let rec partition_factors
  xs = (let n =
          divide_nata (sum_list monoid_add_nat (map snd xs))
            (nat_of_integer (Z.of_int 2))
          in
         (match partition_factors_main n xs with ([], []) -> ([], [])
           | ([], [x]) -> ([], [x]) | ([], x :: y :: ys) -> ([x], y :: ys)
           | ([x], b) -> ([x], b) | (x :: y :: ys, []) -> ([x], y :: ys)
           | (x :: y :: ys, ad :: listb) -> (x :: y :: ys, ad :: listb)));;

let rec create_factor_tree_balanced
  xs = (if less_eq_nat (size_list xs) one_nata
         then Factor_Leaf ((), fst (hda xs))
         else (let (l, r) = partition_factors xs in
                Factor_Node
                  ((), create_factor_tree_balanced l,
                    create_factor_tree_balanced r)));;

let rec sequences _B
  key x1 = match key, x1 with
    key, a :: b :: xs ->
      (if less _B.order_linorder.preorder_order.ord_preorder (key b) (key a)
        then desc _B key b [a] xs else asc _B key b (fun ba -> a :: ba) xs)
    | key, [x] -> [[x]]
    | key, [] -> []
and asc _B
  key a asa x3 = match key, a, asa, x3 with
    key, a, asa, b :: bs ->
      (if less_eq _B.order_linorder.preorder_order.ord_preorder (key a) (key b)
        then asc _B key b (fun ys -> asa (a :: ys)) bs
        else asa [a] :: sequences _B key (b :: bs))
    | key, a, asa, [] -> [asa [a]]
and desc _B
  key a asa x3 = match key, a, asa, x3 with
    key, a, asa, b :: bs ->
      (if less _B.order_linorder.preorder_order.ord_preorder (key b) (key a)
        then desc _B key b (a :: asa) bs
        else (a :: asa) :: sequences _B key (b :: bs))
    | key, a, asa, [] -> [a :: asa];;

let rec mergea _B
  key x1 bs = match key, x1, bs with
    key, a :: asa, b :: bs ->
      (if less _B.order_linorder.preorder_order.ord_preorder (key b) (key a)
        then b :: mergea _B key (a :: asa) bs
        else a :: mergea _B key asa (b :: bs))
    | key, [], bs -> bs
    | key, v :: va, [] -> v :: va;;

let rec merge_pairs _B
  key x1 = match key, x1 with
    key, a :: b :: xs -> mergea _B key a b :: merge_pairs _B key xs
    | key, [] -> []
    | key, [v] -> [v];;

let rec merge_all _B
  key x1 = match key, x1 with key, [] -> []
    | key, [x] -> x
    | key, v :: vb :: vc ->
        merge_all _B key (merge_pairs _B key (v :: vb :: vc));;

let rec msort_key _B key xs = merge_all _B key (sequences _B key xs);;

let rec sort_key _B key = msort_key _B key;;

let rec create_factor_tree
  xs = (let ys = map (fun f -> (f, degreea zero_int f)) xs in
        let a = rev (sort_key linorder_nat snd ys) in
         create_factor_tree_balanced a);;

let rec hensel_lifting_monic
  p n u vs =
    (if null vs then []
      else (let pn = binary_power monoid_mult_int p n in
            let c = mp pn u in
            let a = product_factor_tree p (create_factor_tree vs) in
             hensel_lifting_main p n c a));;

let rec euclid_ext_aux (_A1, _A2)
  sa s ta t ra r =
    (if eq _A2 r
          (zero _A1.factorial_ring_gcd_euclidean_ring_gcd.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd)
      then (let c =
              divide
                _A1.euclidean_ring_euclidean_ring_gcd.idom_modulo_euclidean_ring.semidom_modulo_idom_modulo.semiring_modulo_semidom_modulo.modulo_semiring_modulo.divide_modulo
                (one _A1.factorial_ring_gcd_euclidean_ring_gcd.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.one_gcd)
                (unit_factor
                  _A1.factorial_ring_gcd_euclidean_ring_gcd.ring_gcd_factorial_ring_gcd.semiring_gcd_ring_gcd.normalization_semidom_semiring_gcd.semidom_divide_unit_factor_normalization_semidom.unit_factor_semidom_divide_unit_factor
                  ra)
              in
             ((times _A1.factorial_ring_gcd_euclidean_ring_gcd.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.dvd_gcd.times_dvd
                 sa c,
                times _A1.factorial_ring_gcd_euclidean_ring_gcd.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.dvd_gcd.times_dvd
                  ta c),
               normalizea
                 _A1.factorial_ring_gcd_euclidean_ring_gcd.ring_gcd_factorial_ring_gcd.semiring_gcd_ring_gcd.normalization_semidom_semiring_gcd
                 ra))
      else (let q =
              divide
                _A1.euclidean_ring_euclidean_ring_gcd.idom_modulo_euclidean_ring.semidom_modulo_idom_modulo.semiring_modulo_semidom_modulo.modulo_semiring_modulo.divide_modulo
                ra r
              in
             euclid_ext_aux (_A1, _A2) s
               (minus
                 _A1.euclidean_ring_euclidean_ring_gcd.idom_modulo_euclidean_ring.idom_divide_idom_modulo.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.group_add_neg_numeral.minus_group_add
                 sa (times
                      _A1.factorial_ring_gcd_euclidean_ring_gcd.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.dvd_gcd.times_dvd
                      q s))
               t (minus
                   _A1.euclidean_ring_euclidean_ring_gcd.idom_modulo_euclidean_ring.idom_divide_idom_modulo.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.group_add_neg_numeral.minus_group_add
                   ta (times
                        _A1.factorial_ring_gcd_euclidean_ring_gcd.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.dvd_gcd.times_dvd
                        q t))
               r (modulo
                   _A1.euclidean_ring_euclidean_ring_gcd.idom_modulo_euclidean_ring.semidom_modulo_idom_modulo.semiring_modulo_semidom_modulo.modulo_semiring_modulo
                   ra r)));;

let rec bezout_coefficients (_A1, _A2)
  a b = fst (euclid_ext_aux (_A1, _A2)
              (one _A1.factorial_ring_gcd_euclidean_ring_gcd.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.one_gcd)
              (zero _A1.factorial_ring_gcd_euclidean_ring_gcd.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd)
              (zero _A1.factorial_ring_gcd_euclidean_ring_gcd.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd)
              (one _A1.factorial_ring_gcd_euclidean_ring_gcd.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.one_gcd)
              a b);;

let rec inverse_mod
  x m = fst (bezout_coefficients (euclidean_ring_gcd_int, equal_int) x m);;

let rec hensel_lifting
  p n f gs =
    (let lc = coeffa zero_int f (degreea zero_int f) in
     let ilc = inverse_mod lc (binary_power monoid_mult_int p n) in
     let g =
       smult (equal_int, comm_semiring_0_int, semiring_no_zero_divisors_int) ilc
         f
       in
      hensel_lifting_monic p n g gs);;

let rec root_int_maina
  pm ipm ip x n =
    (let xpm = binary_power monoid_mult_int x pm in
     let xp = times_inta xpm x in
      (if less_eq_int xp n then (x, equal_inta xp n)
        else root_int_maina pm ipm ip
               (divide_inta (plus_inta (divide_inta n xpm) (times_inta x ipm))
                 ip)
               n));;

let rec numeral _A
  = function
    Bit1 n ->
      (let m = numeral _A n in
        plus _A.semigroup_add_numeral.plus_semigroup_add
          (plus _A.semigroup_add_numeral.plus_semigroup_add m m)
          (one _A.one_numeral))
    | Bit0 n ->
        (let m = numeral _A n in
          plus _A.semigroup_add_numeral.plus_semigroup_add m m)
    | One -> one _A.one_numeral;;

let rec of_nat _A
  n = (if equal_nata n zero_nata
        then zero _A.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
        else (let (m, q) = divmod_nat n (nat_of_integer (Z.of_int 2)) in
              let ma =
                times _A.semiring_numeral_semiring_1.monoid_mult_semiring_numeral.power_monoid_mult.times_power
                  (numeral
                    _A.semiring_numeral_semiring_1.numeral_semiring_numeral
                    (Bit0 One))
                  (of_nat _A m)
                in
               (if equal_nata q zero_nata then ma
                 else plus _A.semiring_numeral_semiring_1.numeral_semiring_numeral.semigroup_add_numeral.plus_semigroup_add
                        ma (one _A.semiring_numeral_semiring_1.numeral_semiring_numeral.one_numeral))));;

let rec ceiling _A
  x = uminus_inta
        (floor _A
          (uminus
            _A.archimedean_field_floor_ceiling.linordered_field_archimedean_field.linordered_idom_linordered_field.linordered_ring_strict_linordered_idom.linordered_ring_linordered_ring_strict.abs_if_linordered_ring.uminus_abs_if
            x));;

type proper_base = Abs_proper_base of int;;

let rec into_base
  xa = Abs_proper_base
         (if less_eq_int (Int_of_integer (Z.of_int 2)) xa then xa
           else Int_of_integer (Z.of_int 2));;

let rec rep_proper_base (Abs_proper_base x) = x;;

let rec square_base
  xa = Abs_proper_base (times_inta (rep_proper_base xa) (rep_proper_base xa));;

let rec get_base x = rep_proper_base x;;

let rec log_main
  b x = (if less_int x (get_base b) then (zero_nata, one_inta)
          else (let (z, bz) = log_main (square_base b) x in
                let l = times_nata (nat_of_integer (Z.of_int 2)) z in
                let bz1 = times_inta bz (get_base b) in
                 (if less_int x bz1 then (l, bz) else (suc l, bz1))));;

let rec log_ceiling
  b x = (let (y, by) = log_main (into_base b) x in
          (if equal_inta x by then y else suc y));;

let rec start_value
  n p = binary_power monoid_mult_int (Int_of_integer (Z.of_int 2))
          (nat (ceiling floor_ceiling_rat
                 (divide_rata
                   (of_nat semiring_1_rat
                     (log_ceiling (Int_of_integer (Z.of_int 2)) n))
                   (of_nat semiring_1_rat p))));;

let rec root_int_main
  p n = (if equal_nata p zero_nata then (one_inta, equal_inta n one_inta)
          else (let pm = minus_nata p one_nata in
                 root_int_maina pm (int_of_nat pm) (int_of_nat p)
                   (start_value n p) n));;

let rec root_int_ceiling_pos
  p x = (if equal_nata p zero_nata then zero_inta
          else (match root_int_main p x with (y, true) -> y
                 | (y, false) -> plus_inta y one_inta));;

let rec root_int_floor_pos
  p x = (if equal_nata p zero_nata then zero_inta
          else fst (root_int_main p x));;

let rec root_int_floor
  p x = (if less_eq_int zero_inta x then root_int_floor_pos p x
          else uminus_inta (root_int_ceiling_pos p (uminus_inta x)));;

let rec mahler_landau_graeffe_approximation
  kk dd f =
    (let no =
       sum_list monoid_add_int
         (map (fun a -> times_inta a a) (coeffs zero_int f))
       in
      root_int_floor kk (times_inta (int_of_nat dd) no));;

let rec alternate
  = function
    x :: y :: ys -> (let (evn, od) = alternate ys in (x :: evn, y :: od))
    | [] -> ([], [])
    | [v] -> ([v], []);;

let rec poly_even_odd (_A1, _A2)
  f = (let (evn, od) =
         alternate
           (coeffs
             _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
             f)
         in
        (poly_of_list
           (_A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.comm_monoid_add_semiring_0,
             _A1)
           evn,
          poly_of_list
            (_A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.comm_monoid_add_semiring_0,
              _A1)
            od));;

let rec graeffe_one_step (_A1, _A2)
  c f = (let (g, h) = poly_even_odd (_A1, _A2.comm_ring_1_idom) f in
          smult (_A1, _A2.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_semiring_0_comm_semiring_1,
                  _A2.semidom_idom.semiring_1_no_zero_divisors_semidom.semiring_no_zero_divisors_semiring_1_no_zero_divisors)
            c (minus_polya
                (_A2.comm_ring_1_idom.ring_1_comm_ring_1.ring_ring_1.ab_group_add_ring,
                  _A1)
                (karatsuba_mult_poly
                  (_A1, _A2.comm_ring_1_idom,
                    _A2.semidom_idom.semiring_1_no_zero_divisors_semidom.semiring_no_zero_divisors_semiring_1_no_zero_divisors)
                  g g)
                (karatsuba_mult_poly
                  (_A1, _A2.comm_ring_1_idom,
                    _A2.semidom_idom.semiring_1_no_zero_divisors_semidom.semiring_no_zero_divisors_semiring_1_no_zero_divisors)
                  (monom_mult
                    _A2.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel
                    one_nata h)
                  h)));;

let rec mahler_approximation_main
  bnd dd c g mm k kk =
    (let mmm = mahler_landau_graeffe_approximation kk dd g in
     let new_mm = (if equal_nata k zero_nata then mmm else min ord_int mm mmm)
       in
      (if less_eq_nat bnd k then new_mm
        else mahler_approximation_main bnd (times_nata dd dd) c
               (graeffe_one_step (equal_int, idom_int) c g) new_mm (suc k)
               (times_nata (nat_of_integer (Z.of_int 2)) kk)));;

let rec mahler_approximation
  bnd d f =
    mahler_approximation_main bnd (times_nata d d)
      (binary_power monoid_mult_int (uminus_inta one_inta) (degreea zero_int f))
      f (uminus_inta one_inta) zero_nata (nat_of_integer (Z.of_int 2));;

let rec fold_atLeastAtMost_nat
  f a b acc =
    (if less_nat b a then acc
      else fold_atLeastAtMost_nat f (plus_nata a one_nata) b (f a acc));;

let rec fact _A
  n = of_nat _A.semiring_1_semiring_char_0
        (fold_atLeastAtMost_nat times_nata (nat_of_integer (Z.of_int 2)) n
          one_nata);;

let rec binomial
  n k = (if less_eq_nat k n
          then divide_nata (fact semiring_char_0_nat n)
                 (times_nata (fact semiring_char_0_nat k)
                   (fact semiring_char_0_nat (minus_nata n k)))
          else zero_nata);;

let rec mignotte_bound
  f d = (let da = minus_nata d one_nata in
         let d2 = divide_nata da (nat_of_integer (Z.of_int 2)) in
         let binom = binomial da d2 in
          plus_inta (mahler_approximation (nat_of_integer (Z.of_int 2)) binom f)
            (times_inta (int_of_nat binom)
              (abs_int (coeffa zero_int f (degreea zero_int f)))));;

let rec factor_bound x = mignotte_bound x;;

let rec drop
  n x1 = match n, x1 with n, [] -> []
    | n, x :: xs ->
        (if equal_nata n zero_nata then x :: xs
          else drop (minus_nata n one_nata) xs);;

let rec max_factor_degree
  degs =
    (let ds = sort_key linorder_nat (fun x -> x) degs in
      sum_list monoid_add_nat
        (drop (divide_nata (size_list ds) (nat_of_integer (Z.of_int 2))) ds));;

let rec degree_bound _A vs = max_factor_degree (map (degreea _A) vs);;

let rec berlekamp_zassenhaus_factorization
  f = (let p = suitable_prime_bz f in
       let (_, fs) = finite_field_factorization_int p f in
       let max_deg = degree_bound zero_int fs in
       let bnd =
         times_inta
           (times_inta (Int_of_integer (Z.of_int 2))
             (abs_int (coeffa zero_int f (degreea zero_int f))))
           (factor_bound f max_deg)
         in
       let k = find_exponent p bnd in
       let vs = hensel_lifting p k f fs in
        zassenhaus_reconstruction vs p k f);;

let berlekamp_zassenhaus_factorization_algorithm :
  int_poly_factorization_algorithm
  = Abs_int_poly_factorization_algorithm berlekamp_zassenhaus_factorization;;

let rec yun_factorization_main (_A1, _A2)
  gcd bn cn i sqr =
    (if eq (equal_poly
             (_A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd,
               _A2))
          bn (one_polya
               _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel)
      then sqr
      else (let dn =
              minus_polya
                (_A1.idom_divide_factorial_ring_gcd.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.ring_ring_1.ab_group_add_ring,
                  _A2)
                cn (pderiv
                     (_A2, _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel,
                       _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.semiring_1_no_zero_divisors_semidom.semiring_no_zero_divisors_semiring_1_no_zero_divisors)
                     bn)
              in
            let an = gcd bn dn in
             yun_factorization_main (_A1, _A2) gcd
               (divide_polya (_A2, _A1.idom_divide_factorial_ring_gcd) bn an)
               (divide_polya (_A2, _A1.idom_divide_factorial_ring_gcd) dn an)
               (suc i) ((an, i) :: sqr)));;

let rec yun_monic_factorization (_A1, _A2)
  gcd p =
    (let pp =
       pderiv
         (_A2, _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel,
           _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.semiring_1_no_zero_divisors_semidom.semiring_no_zero_divisors_semiring_1_no_zero_divisors)
         p
       in
     let u = gcd p pp in
     let b0 = divide_polya (_A2, _A1.idom_divide_factorial_ring_gcd) p u in
     let c0 = divide_polya (_A2, _A1.idom_divide_factorial_ring_gcd) pp u in
      filtera
        (fun (a, _) ->
          not (eq (equal_poly
                    (_A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd,
                      _A2))
                a (one_polya
                    _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel)))
        (yun_factorization_main (_A1, _A2) gcd b0 c0 zero_nata []));;

let rec find uu x1 = match uu, x1 with uu, [] -> None
               | p, x :: xs -> (if p x then Some x else find p xs);;

let rec square_free_heuristic
  f = (let lc = coeffa zero_int f (degreea zero_int f) in
        find (fun p ->
               coprimea (semiring_gcd_int, equal_int) lc p &&
                 separable_impl p f)
          [Int_of_integer (Z.of_int 2); Int_of_integer (Z.of_int 3);
            Int_of_integer (Z.of_int 5); Int_of_integer (Z.of_int 7);
            Int_of_integer (Z.of_int 11); Int_of_integer (Z.of_int 13);
            Int_of_integer (Z.of_int 17); Int_of_integer (Z.of_int 19);
            Int_of_integer (Z.of_int 23)]);;

let rec normalize_poly (_A1, _A2, _A3)
  p = divide_polya (_A1, _A2) p
        (pCons
          (_A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
            _A1)
          (unit_factor _A3.unit_factor_semidom_divide_unit_factor
            (coeffa
              _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
              p (degreea
                  _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
                  p)))
          (zero_polya
            _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero));;

let rec coprime_approx_main _A
  p ff_ops f g =
    equal_lista _A
      (gcd_poly_i _A ff_ops (of_int_poly_i ff_ops (mp p f))
        (of_int_poly_i ff_ops (mp p g)))
      [onea ff_ops];;

let gcd_primes64 : int list
  = [Int_of_integer (Z.of_int 383); Int_of_integer (Z.of_int 21984191);
      Int_of_integer (Z.of_int 50329901); Int_of_integer (Z.of_int 80329901);
      Int_of_integer (Z.of_int 219849193)];;

let rec coprime_heuristic
  f g = (let lcf = coeffa zero_int f (degreea zero_int f) in
         let lcg = coeffa zero_int g (degreea zero_int g) in
          not (is_none
                (find (fun p ->
                        (coprimea (semiring_gcd_int, equal_int) lcf p ||
                          coprimea (semiring_gcd_int, equal_int) lcg p) &&
                          coprime_approx_main equal_uint64 p
                            (finite_field_ops64 (uint64_of_int p)) f g)
                  gcd_primes64)));;

let rec pseudo_mod_main_list (_A1, _A2)
  lc r d n =
    (if equal_nata n zero_nata then r
      else (let rr =
              map (times
                    _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd
                    lc)
                r
              in
            let a = hda r in
            let rrr =
              tla (if eq _A1 a
                        (zero _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
                    then rr
                    else minus_poly_rev_list
                           _A2.ring_1_comm_ring_1.neg_numeral_ring_1.group_add_neg_numeral
                           rr (map (times
                                     _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd
                                     a)
                                d))
              in
             pseudo_mod_main_list (_A1, _A2) lc rrr d
               (minus_nata n one_nata)));;

let rec pseudo_mod_list (_A1, _A2)
  p q = (if null q then p
          else (let rq = rev q in
                let a =
                  pseudo_mod_main_list (_A1, _A2) (hda rq) (rev p) rq
                    (minus_nata (plus_nata one_nata (size_list p))
                      (size_list q))
                  in
                 rev a));;

let rec pseudo_mod (_A1, _A2, _A3)
  f g = poly_of_list
          (_A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.comm_monoid_add_semiring_0,
            _A1)
          (pseudo_mod_list (_A1, _A2)
            (coeffs
              _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
              f)
            (coeffs
              _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
              g));;

let rec is_zero _A p = null (coeffs _A p);;

let rec gcd_poly_code_aux (_A1, _A2)
  p q = (if is_zero
              _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
              q
          then normalize_poly
                 (_A2, _A1.idom_divide_factorial_ring_gcd,
                   _A1.ring_gcd_factorial_ring_gcd.semiring_gcd_ring_gcd.normalization_semidom_semiring_gcd.semidom_divide_unit_factor_normalization_semidom)
                 p
          else gcd_poly_code_aux (_A1, _A2) q
                 (primitive_part
                   (_A1.ring_gcd_factorial_ring_gcd.semiring_gcd_ring_gcd, _A2)
                   (pseudo_mod
                     (_A2, _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.comm_ring_1_idom,
                       _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.semiring_1_no_zero_divisors_semidom)
                     p q)));;

let rec gcd_int_poly
  f g = (if equal_polya (zero_int, equal_int) f (zero_polya zero_int)
          then normalize_poly
                 (equal_int, idom_divide_int, semidom_divide_unit_factor_int) g
          else (if equal_polya (zero_int, equal_int) g (zero_polya zero_int)
                 then normalize_poly
                        (equal_int, idom_divide_int,
                          semidom_divide_unit_factor_int)
                        f
                 else (let cf = content semiring_gcd_int f in
                       let cg = content semiring_gcd_int g in
                       let ct = gcd_intc cf cg in
                       let ff =
                         map_poly zero_int (zero_int, equal_int)
                           (fun x -> divide_inta x cf) f
                         in
                       let gg =
                         map_poly zero_int (zero_int, equal_int)
                           (fun x -> divide_inta x cg) g
                         in
                        (if coprime_heuristic ff gg
                          then pCons (zero_int, equal_int) ct
                                 (zero_polya zero_int)
                          else smult (equal_int, comm_semiring_0_int,
                                       semiring_no_zero_divisors_int)
                                 ct (gcd_poly_code_aux
                                      (factorial_ring_gcd_int, equal_int) ff
                                      gg)))));;

let rec square_free_factorization_int_main
  f = (match square_free_heuristic f
        with None ->
          yun_monic_factorization (factorial_ring_gcd_int, equal_int)
            gcd_int_poly f
        | Some _ -> [(f, zero_nata)]);;

let rec square_free_factorization_inta
  f = (if equal_nata (degreea zero_int f) zero_nata
        then (coeffa zero_int f (degreea zero_int f), [])
        else (let c = content semiring_gcd_int f in
              let d =
                times_inta (sgn_int (coeffa zero_int f (degreea zero_int f))) c
                in
              let g = sdiv_poly (equal_int, idom_divide_int) f d in
               (d, square_free_factorization_int_main g)));;

let rec takeWhile p x1 = match p, x1 with p, [] -> []
                    | p, x :: xs -> (if p x then x :: takeWhile p xs else []);;

let rec x_split (_A1, _A2)
  f = (let fs = coeffs _A2.mult_zero_semiring_0.zero_mult_zero f in
       let zs =
         takeWhile (eq _A1 (zero _A2.mult_zero_semiring_0.zero_mult_zero)) fs in
        (match zs with [] -> (zero_nata, f)
          | _ :: _ ->
            (size_list zs,
              poly_of_list (_A2.comm_monoid_add_semiring_0, _A1)
                (dropWhile
                  (eq _A1 (zero _A2.mult_zero_semiring_0.zero_mult_zero))
                  fs))));;

let rec monoma (_A1, _A2)
  a n = Poly (if eq _A2 a (zero _A1) then []
               else replicate n (zero _A1) @ [a]);;

let rec square_free_factorization_int
  f = (let (n, g) = x_split (equal_int, semiring_0_int) f in
       let (d, fs) = square_free_factorization_inta g in
        (if equal_nata n zero_nata then (d, fs)
          else (d, (monoma (zero_int, equal_int) one_inta one_nata,
                     minus_nata n one_nata) ::
                     fs)));;

let rec rep_int_poly_factorization_algorithm
  (Abs_int_poly_factorization_algorithm x) = x;;

let rec int_poly_factorization_algorithm
  x = rep_int_poly_factorization_algorithm x;;

let rec reflect_poly (_A1, _A2)
  p = Poly (rev (dropWhile (eq _A2 (zero _A1)) (coeffs _A1 p)));;

let rec main_int_poly_factorization
  alg f =
    (let df = degreea zero_int f in
      (if equal_nata df one_nata then [f]
        else (if less_int
                   (abs_int
                     (match coeffs zero_int f with [] -> zero_inta
                       | x :: _ -> x))
                   (abs_int (coeffa zero_int f df))
               then map (reflect_poly (zero_int, equal_int))
                      (int_poly_factorization_algorithm alg
                        (reflect_poly (zero_int, equal_int) f))
               else int_poly_factorization_algorithm alg f)));;

let rec internal_int_poly_factorization
  alg f =
    (let (a, gis) = square_free_factorization_int f in
      (a, maps (fun (g, i) ->
                 map (fun fa -> (fa, i)) (main_int_poly_factorization alg g))
            gis));;

let rec factorize_int_last_nz_poly
  alg f =
    (let df = degreea zero_int f in
      (if equal_nata df zero_nata
        then ((match coeffs zero_int f with [] -> zero_inta | x :: _ -> x), [])
        else (if equal_nata df one_nata
               then (content semiring_gcd_int f,
                      [(primitive_part (semiring_gcd_int, equal_int) f,
                         zero_nata)])
               else internal_int_poly_factorization alg f)));;

let rec factorize_int_poly_generic
  alg f =
    (let (n, g) = x_split (equal_int, semiring_0_int) f in
      (if equal_polya (zero_int, equal_int) g (zero_polya zero_int)
        then (zero_inta, [])
        else (let (a, fs) = factorize_int_last_nz_poly alg g in
               (if equal_nata n zero_nata then (a, fs)
                 else (a, (monoma (zero_int, equal_int) one_inta one_nata,
                            minus_nata n one_nata) ::
                            fs)))));;

let rec factors_of_int_poly
  p = map (comp abs_int_poly fst)
        (snd (factorize_int_poly_generic
               berlekamp_zassenhaus_factorization_algorithm p));;

let rec real_alg_2a
  ri p l r =
    (if equal_nata (degreea zero_int p) one_nata
      then Rational
             (fract
               (uminus_inta
                 (match coeffs zero_int p with [] -> zero_inta | x :: _ -> x))
               (coeffa zero_int p one_nata))
      else (let (pa, (la, ra)) =
              normalize_bounds_1
                (let (la, (ra, _)) =
                   tighten_poly_bounds_for_x p zero_rata l r
                     (sgn_rata
                       (fold_coeffs zero_int
                         (fun a b -> plus_rata (of_int a) (times_rata r b)) p
                         zero_rata))
                   in
                  (p, (la, ra)))
              in
             Irrational (number_root ri ra, (pa, (la, ra)))));;

let rec select_correct_factor_int_poly
  bnd_update bnd_get init p =
    (let qs = factors_of_int_poly p in
     let polys = map (fun q -> (q, root_info q)) qs in
     let (a, b) = select_correct_factor bnd_update bnd_get init polys in
      (let (q, ri) = a in (fun (aa, ba) -> real_alg_2a ri q aa ba)) b);;

let rec tighten_poly_bounds_binary
  cr1 cr2 ((l1, (r1, sr1)), (l2, (r2, sr2))) =
    (tighten_poly_bounds cr1 l1 r1 sr1, tighten_poly_bounds cr2 l2 r2 sr2);;

let rec zero_poly _A = ({zero = zero_polya _A} : 'a poly zero);;

let rec poly_lift (_A1, _A2)
  = map_poly _A1 ((zero_poly _A1), (equal_poly (_A1, _A2)))
      (fun a -> pCons (_A1, _A2) a (zero_polya _A1));;

let rec plus_poly (_A1, _A2) = ({plus = plus_polya (_A1, _A2)} : 'a poly plus);;

let rec semigroup_add_poly (_A1, _A2) =
  ({plus_semigroup_add = (plus_poly (_A1, _A2))} : 'a poly semigroup_add);;

let rec ab_semigroup_add_poly (_A1, _A2) =
  ({semigroup_add_ab_semigroup_add = (semigroup_add_poly (_A1, _A2))} :
    'a poly ab_semigroup_add);;

let rec monoid_add_poly (_A1, _A2) =
  ({semigroup_add_monoid_add = (semigroup_add_poly (_A1, _A2));
     zero_monoid_add =
       (zero_poly _A1.monoid_add_comm_monoid_add.zero_monoid_add)}
    : 'a poly monoid_add);;

let rec comm_monoid_add_poly (_A1, _A2) =
  ({ab_semigroup_add_comm_monoid_add = (ab_semigroup_add_poly (_A1, _A2));
     monoid_add_comm_monoid_add = (monoid_add_poly (_A1, _A2))}
    : 'a poly comm_monoid_add);;

let rec poly_x_mult_y (_A1, _A2)
  p = (let cs = coeffs _A1.monoid_add_comm_monoid_add.zero_monoid_add p in
        poly_of_list
          ((comm_monoid_add_poly (_A1, _A2)),
            (equal_poly (_A1.monoid_add_comm_monoid_add.zero_monoid_add, _A2)))
          (map (fun (i, ai) ->
                 monoma (_A1.monoid_add_comm_monoid_add.zero_monoid_add, _A2) ai
                   i)
            (zip (upt zero_nata (size_list cs)) cs)));;

let rec dichotomous_Lazard _A
  x y n =
    (if less_eq_nat n one_nata
      then (if equal_nata n one_nata then x
             else one _A.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.numeral_neg_numeral.one_numeral)
      else (let (d, r) = divmod_nat n (nat_of_integer (Z.of_int 2)) in
            let reca = dichotomous_Lazard _A x y d in
            let recsq =
              divide _A.semidom_divide_idom_divide.divide_semidom_divide
                (times
                  _A.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd
                  reca reca)
                y
              in
             (if equal_nata r zero_nata then recsq
               else divide _A.semidom_divide_idom_divide.divide_semidom_divide
                      (times
                        _A.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd
                        recsq x)
                      y)));;

let rec resultant_impl_rec_Lazard (_A1, _A2)
  gi_1 gi ni_1 d1_1 hi_2 =
    (let ni =
       degreea
         _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
         gi
       in
     let pmod =
       pseudo_mod
         (_A2, _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.comm_ring_1_idom,
           _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.semiring_1_no_zero_divisors_semidom)
         gi_1 gi
       in
      (if is_zero
            _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
            pmod
        then (if equal_nata ni zero_nata
               then (let d1 = minus_nata ni_1 ni in
                     let gia =
                       coeffa
                         _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
                         gi (degreea
                              _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
                              gi)
                       in
                      (if equal_nata d1 one_nata then gia
                        else (let gi_1a =
                                coeffa
                                  _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
                                  gi_1
                                  (degreea
                                    _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
                                    gi_1)
                                in
                              let hi_1 =
                                (if equal_nata d1_1 one_nata then gi_1a
                                  else dichotomous_Lazard
 _A1.idom_divide_factorial_ring_gcd gi_1a hi_2 d1_1)
                                in
                               dichotomous_Lazard
                                 _A1.idom_divide_factorial_ring_gcd gia hi_1
                                 d1)))
               else zero _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd)
        else (let d1 = minus_nata ni_1 ni in
              let gi_1a =
                coeffa
                  _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
                  gi_1
                  (degreea
                    _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
                    gi_1)
                in
              let hi_1 =
                (if equal_nata d1_1 one_nata then gi_1a
                  else dichotomous_Lazard _A1.idom_divide_factorial_ring_gcd
                         gi_1a hi_2 d1_1)
                in
              let divisor =
                (if equal_nata d1 one_nata
                  then times _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.dvd_gcd.times_dvd
                         gi_1a hi_1
                  else (if dvd (equal_nat, semidom_modulo_nat)
                             (nat_of_integer (Z.of_int 2)) d1
                         then times _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.dvd_gcd.times_dvd
                                (uminus
                                  _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.group_add_neg_numeral.uminus_group_add
                                  gi_1a)
                                (binary_power
                                  _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_numeral_semiring_1.monoid_mult_semiring_numeral
                                  hi_1 d1)
                         else times _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.dvd_gcd.times_dvd
                                gi_1a
                                (binary_power
                                  _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_numeral_semiring_1.monoid_mult_semiring_numeral
                                  hi_1 d1)))
                in
              let gi_p1 =
                sdiv_poly (_A2, _A1.idom_divide_factorial_ring_gcd) pmod divisor
                in
               resultant_impl_rec_Lazard (_A1, _A2) gi gi_p1 ni d1 hi_1)));;

let rec resultant_impl_start_Lazard (_A1, _A2)
  g1 g2 =
    (let pmod =
       pseudo_mod
         (_A2, _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.comm_ring_1_idom,
           _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.semiring_1_no_zero_divisors_semidom)
         g1 g2
       in
     let n2 =
       degreea
         _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
         g2
       in
     let n1 =
       degreea
         _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
         g1
       in
     let g2a =
       coeffa
         _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
         g2 (degreea
              _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
              g2)
       in
     let d1 = minus_nata n1 n2 in
      (if is_zero
            _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
            pmod
        then (if equal_nata n2 zero_nata
               then (if equal_nata d1 zero_nata
                      then one _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.one_gcd
                      else (if equal_nata d1 one_nata then g2a
                             else binary_power
                                    _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_numeral_semiring_1.monoid_mult_semiring_numeral
                                    g2a d1))
               else zero _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd)
        else (let g3 =
                (if dvd (equal_nat, semidom_modulo_nat)
                      (nat_of_integer (Z.of_int 2)) d1
                  then uminus_polya
                         _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.ring_ring_1.ab_group_add_ring
                         pmod
                  else pmod)
                in
              let n3 =
                degreea
                  _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
                  g3
                in
              let pmoda =
                pseudo_mod
                  (_A2, _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.comm_ring_1_idom,
                    _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.semiring_1_no_zero_divisors_semidom)
                  g2 g3
                in
               (if is_zero
                     _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
                     pmoda
                 then (if equal_nata n3 zero_nata
                        then (let d2 = minus_nata n2 n3 in
                              let g3a =
                                coeffa
                                  _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
                                  g3 (degreea
                                       _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
                                       g3)
                                in
                               (if equal_nata d2 one_nata then g3a
                                 else dichotomous_Lazard
_A1.idom_divide_factorial_ring_gcd g3a
(if equal_nata d1 one_nata then g2a
  else binary_power
         _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_numeral_semiring_1.monoid_mult_semiring_numeral
         g2a d1)
d2))
                        else zero _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd)
                 else (let h2 =
                         (if equal_nata d1 one_nata then g2a
                           else binary_power
                                  _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_numeral_semiring_1.monoid_mult_semiring_numeral
                                  g2a d1)
                         in
                       let d2 = minus_nata n2 n3 in
                       let divisor =
                         (if equal_nata d2 one_nata
                           then times _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.dvd_gcd.times_dvd
                                  g2a h2
                           else (if dvd (equal_nat, semidom_modulo_nat)
                                      (nat_of_integer (Z.of_int 2)) d2
                                  then times
 _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.dvd_gcd.times_dvd
 (uminus
   _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.group_add_neg_numeral.uminus_group_add
   g2a)
 (binary_power
   _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_numeral_semiring_1.monoid_mult_semiring_numeral
   h2 d2)
                                  else times
 _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.dvd_gcd.times_dvd
 g2a (binary_power
       _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_numeral_semiring_1.monoid_mult_semiring_numeral
       h2 d2)))
                         in
                       let g4 =
                         sdiv_poly (_A2, _A1.idom_divide_factorial_ring_gcd)
                           pmoda divisor
                         in
                        resultant_impl_rec_Lazard (_A1, _A2) g3 g4 n3 d2
                          h2)))));;

let rec resultant_impl_main_Lazard (_A1, _A2)
  g1 g2 =
    (if is_zero
          _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
          g2
      then (if equal_nata
                 (degreea
                   _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
                   g1)
                 zero_nata
             then one _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.one_gcd
             else zero _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd)
      else resultant_impl_start_Lazard (_A1, _A2) g1 g2);;

let rec resultant_impl_Lazard (_A1, _A2)
  f g = (if less_eq_nat
              (size_list
                (coeffs
                  _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
                  g))
              (size_list
                (coeffs
                  _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
                  f))
          then resultant_impl_main_Lazard (_A1, _A2) f g
          else (let res = resultant_impl_main_Lazard (_A1, _A2) g f in
                 (if dvd (equal_nat, semidom_modulo_nat)
                       (nat_of_integer (Z.of_int 2))
                       (degreea
                         _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
                         f) ||
                       dvd (equal_nat, semidom_modulo_nat)
                         (nat_of_integer (Z.of_int 2))
                         (degreea
                           _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
                           g)
                   then res
                   else uminus
                          _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.group_add_neg_numeral.uminus_group_add
                          res)));;

let rec resultant (_A1, _A2) = resultant_impl_Lazard (_A1, _A2);;

let rec unit_factor_polya (_A1, _A2, _A3)
  p = pCons (_A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
              _A1)
        (unit_factor _A3.unit_factor_semidom_divide_unit_factor
          (coeffa
            _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
            p (degreea
                _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
                p)))
        (zero_polya
          _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero);;

let rec gcd_poly_code (_A1, _A2)
  p q = (if is_zero
              _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
              p
          then normalize_poly
                 (_A2, _A1.idom_divide_factorial_ring_gcd,
                   _A1.ring_gcd_factorial_ring_gcd.semiring_gcd_ring_gcd.normalization_semidom_semiring_gcd.semidom_divide_unit_factor_normalization_semidom)
                 q
          else (if is_zero
                     _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
                     q
                 then normalize_poly
                        (_A2, _A1.idom_divide_factorial_ring_gcd,
                          _A1.ring_gcd_factorial_ring_gcd.semiring_gcd_ring_gcd.normalization_semidom_semiring_gcd.semidom_divide_unit_factor_normalization_semidom)
                        p
                 else (let c1 =
                         content
                           _A1.ring_gcd_factorial_ring_gcd.semiring_gcd_ring_gcd
                           p
                         in
                       let c2 =
                         content
                           _A1.ring_gcd_factorial_ring_gcd.semiring_gcd_ring_gcd
                           q
                         in
                       let pa =
                         map_poly
                           _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
                           (_A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd,
                             _A2)
                           (fun x ->
                             divide
                               _A1.idom_divide_factorial_ring_gcd.semidom_divide_idom_divide.divide_semidom_divide
                               x c1)
                           p
                         in
                       let qa =
                         map_poly
                           _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
                           (_A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd,
                             _A2)
                           (fun x ->
                             divide
                               _A1.idom_divide_factorial_ring_gcd.semidom_divide_idom_divide.divide_semidom_divide
                               x c2)
                           q
                         in
                        smult (_A2, _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_semiring_0_comm_semiring_1,
                                _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.semiring_1_no_zero_divisors_semidom.semiring_no_zero_divisors_semiring_1_no_zero_divisors)
                          (gcda _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd
                            c1 c2)
                          (gcd_poly_code_aux (_A1, _A2) pa qa))));;

let rec gcd_polyc (_A1, _A2, _A3) p q = gcd_poly_code (_A1, _A3) p q;;

let rec lcm_polya (_A1, _A2, _A3)
  p q = normalize_poly
          (_A3, _A1.idom_divide_factorial_ring_gcd,
            _A1.ring_gcd_factorial_ring_gcd.semiring_gcd_ring_gcd.normalization_semidom_semiring_gcd.semidom_divide_unit_factor_normalization_semidom)
          (divide_polya (_A3, _A1.idom_divide_factorial_ring_gcd)
            (karatsuba_mult_poly
              (_A3, _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.comm_ring_1_idom,
                _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.semiring_1_no_zero_divisors_semidom.semiring_no_zero_divisors_semiring_1_no_zero_divisors)
              p q)
            (gcd_polyc (_A1, _A2, _A3) p q));;

let rec gcd_polyb (_A1, _A2) x = dummy_Gcd x;;

let rec times_polya (_A1, _A2, _A3)
  p q = fold_coeffs
          _A2.semiring_0_comm_semiring_0.mult_zero_semiring_0.zero_mult_zero
          (fun a pa ->
            plus_polya
              (_A2.semiring_0_comm_semiring_0.comm_monoid_add_semiring_0, _A1)
              (smult (_A1, _A2, _A3) a q)
              (pCons
                (_A2.semiring_0_comm_semiring_0.mult_zero_semiring_0.zero_mult_zero,
                  _A1)
                (zero _A2.semiring_0_comm_semiring_0.mult_zero_semiring_0.zero_mult_zero)
                pa))
          p (zero_polya
              _A2.semiring_0_comm_semiring_0.mult_zero_semiring_0.zero_mult_zero);;

let rec one_poly _A = ({one = one_polya _A} : 'a poly one);;

let rec times_poly (_A1, _A2, _A3) =
  ({times = times_polya (_A1, _A2, _A3)} : 'a poly times);;

let rec dvd_poly (_A1, _A2, _A3) =
  ({times_dvd = (times_poly (_A1, _A2.comm_semiring_0_comm_semiring_1, _A3))} :
    'a poly dvd);;

let rec gcd_polya (_A1, _A2, _A3) =
  ({one_gcd =
      (one_poly
        _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel);
     zero_gcd =
       (zero_poly
         _A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd);
     dvd_gcd =
       (dvd_poly
         (_A3, _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel,
           _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.semiring_1_no_zero_divisors_semidom.semiring_no_zero_divisors_semiring_1_no_zero_divisors));
     gcda = gcd_polyc (_A1, _A2, _A3); lcma = lcm_polya (_A1, _A2, _A3)}
    : 'a poly gcda);;

let rec lcm_poly (_A1, _A2, _A3) x = dummy_Lcm (gcd_poly (_A1, _A2, _A3)) x
and gcd_poly (_A1, _A2, _A3) =
  ({gcd_Gcd = (gcd_polya (_A1, _A2, _A3)); gcd = gcd_polyb (_A1, _A2);
     lcm = lcm_poly (_A1, _A2, _A3)}
    : 'a poly gcd);;

let rec mult_zero_poly (_A1, _A2, _A3) =
  ({times_mult_zero = (times_poly (_A1, _A2, _A3));
     zero_mult_zero =
       (zero_poly
         _A2.semiring_0_comm_semiring_0.mult_zero_semiring_0.zero_mult_zero)}
    : 'a poly mult_zero);;

let rec semigroup_mult_poly (_A1, _A2, _A3) =
  ({times_semigroup_mult = (times_poly (_A1, _A2, _A3))} :
    'a poly semigroup_mult);;

let rec semiring_poly (_A1, _A2, _A3) =
  ({ab_semigroup_add_semiring =
      (ab_semigroup_add_poly
        (_A2.semiring_0_comm_semiring_0.comm_monoid_add_semiring_0, _A1));
     semigroup_mult_semiring = (semigroup_mult_poly (_A1, _A2, _A3))}
    : 'a poly semiring);;

let rec semiring_0_poly (_A1, _A2, _A3) =
  ({comm_monoid_add_semiring_0 =
      (comm_monoid_add_poly
        (_A2.semiring_0_comm_semiring_0.comm_monoid_add_semiring_0, _A1));
     mult_zero_semiring_0 = (mult_zero_poly (_A1, _A2, _A3));
     semiring_semiring_0 = (semiring_poly (_A1, _A2, _A3))}
    : 'a poly semiring_0);;

let rec semiring_no_zero_divisors_poly (_A1, _A2, _A3) =
  ({semiring_0_semiring_no_zero_divisors = (semiring_0_poly (_A1, _A2, _A3))} :
    'a poly semiring_no_zero_divisors);;

let rec semiring_no_zero_divisors_cancel_poly (_A1, _A2) =
  ({semiring_no_zero_divisors_semiring_no_zero_divisors_cancel =
      (semiring_no_zero_divisors_poly
        (_A1, _A2.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_semiring_0_comm_semiring_1,
          _A2.semidom_idom.semiring_1_no_zero_divisors_semidom.semiring_no_zero_divisors_semiring_1_no_zero_divisors))}
    : 'a poly semiring_no_zero_divisors_cancel);;

let rec power_poly (_A1, _A2, _A3) =
  ({one_power = (one_poly _A2);
     times_power = (times_poly (_A1, _A2.comm_semiring_0_comm_semiring_1, _A3))}
    : 'a poly power);;

let rec monoid_mult_poly (_A1, _A2, _A3) =
  ({semigroup_mult_monoid_mult =
      (semigroup_mult_poly (_A1, _A2.comm_semiring_0_comm_semiring_1, _A3));
     power_monoid_mult = (power_poly (_A1, _A2, _A3))}
    : 'a poly monoid_mult);;

let rec numeral_poly (_A1, _A2) =
  ({one_numeral = (one_poly _A2);
     semigroup_add_numeral =
       (semigroup_add_poly
         (_A2.semiring_1_comm_semiring_1.semiring_0_semiring_1.comm_monoid_add_semiring_0,
           _A1))}
    : 'a poly numeral);;

let rec semiring_numeral_poly (_A1, _A2, _A3) =
  ({monoid_mult_semiring_numeral = (monoid_mult_poly (_A1, _A2, _A3));
     numeral_semiring_numeral = (numeral_poly (_A1, _A2));
     semiring_semiring_numeral =
       (semiring_poly (_A1, _A2.comm_semiring_0_comm_semiring_1, _A3))}
    : 'a poly semiring_numeral);;

let rec zero_neq_one_poly _A =
  ({one_zero_neq_one = (one_poly _A);
     zero_zero_neq_one =
       (zero_poly
         _A.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)}
    : 'a poly zero_neq_one);;

let rec semiring_1_poly (_A1, _A2, _A3) =
  ({semiring_numeral_semiring_1 = (semiring_numeral_poly (_A1, _A2, _A3));
     semiring_0_semiring_1 =
       (semiring_0_poly (_A1, _A2.comm_semiring_0_comm_semiring_1, _A3));
     zero_neq_one_semiring_1 = (zero_neq_one_poly _A2)}
    : 'a poly semiring_1);;

let rec semiring_1_no_zero_divisors_poly (_A1, _A2, _A3) =
  ({semiring_1_semiring_1_no_zero_divisors =
      (semiring_1_poly
        (_A1, _A2, _A3.semiring_no_zero_divisors_semiring_1_no_zero_divisors));
     semiring_no_zero_divisors_semiring_1_no_zero_divisors =
       (semiring_no_zero_divisors_poly
         (_A1, _A2.comm_semiring_0_comm_semiring_1,
           _A3.semiring_no_zero_divisors_semiring_1_no_zero_divisors))}
    : 'a poly semiring_1_no_zero_divisors);;

let rec cancel_semigroup_add_poly (_A1, _A2) =
  ({semigroup_add_cancel_semigroup_add =
      (semigroup_add_poly (_A1.comm_monoid_add_cancel_comm_monoid_add, _A2))}
    : 'a poly cancel_semigroup_add);;

let rec minus_poly (_A1, _A2) =
  ({minus = minus_polya (_A1, _A2)} : 'a poly minus);;

let rec cancel_ab_semigroup_add_poly (_A1, _A2) =
  ({ab_semigroup_add_cancel_ab_semigroup_add =
      (ab_semigroup_add_poly
        (_A1.cancel_comm_monoid_add_ab_group_add.comm_monoid_add_cancel_comm_monoid_add,
          _A2));
     cancel_semigroup_add_cancel_ab_semigroup_add =
       (cancel_semigroup_add_poly
         (_A1.cancel_comm_monoid_add_ab_group_add, _A2));
     minus_cancel_ab_semigroup_add = (minus_poly (_A1, _A2))}
    : 'a poly cancel_ab_semigroup_add);;

let rec cancel_comm_monoid_add_poly (_A1, _A2) =
  ({cancel_ab_semigroup_add_cancel_comm_monoid_add =
      (cancel_ab_semigroup_add_poly (_A1, _A2));
     comm_monoid_add_cancel_comm_monoid_add =
       (comm_monoid_add_poly
         (_A1.cancel_comm_monoid_add_ab_group_add.comm_monoid_add_cancel_comm_monoid_add,
           _A2))}
    : 'a poly cancel_comm_monoid_add);;

let rec semiring_0_cancel_poly (_A1, _A2, _A3, _A4) =
  ({cancel_comm_monoid_add_semiring_0_cancel =
      (cancel_comm_monoid_add_poly (_A1, _A2));
     semiring_0_semiring_0_cancel =
       (semiring_0_poly (_A2, _A3.comm_semiring_0_comm_semiring_0_cancel, _A4))}
    : 'a poly semiring_0_cancel);;

let rec ab_semigroup_mult_poly (_A1, _A2, _A3) =
  ({semigroup_mult_ab_semigroup_mult = (semigroup_mult_poly (_A1, _A2, _A3))} :
    'a poly ab_semigroup_mult);;

let rec comm_semiring_poly (_A1, _A2, _A3) =
  ({ab_semigroup_mult_comm_semiring = (ab_semigroup_mult_poly (_A1, _A2, _A3));
     semiring_comm_semiring = (semiring_poly (_A1, _A2, _A3))}
    : 'a poly comm_semiring);;

let rec comm_semiring_0_poly (_A1, _A2, _A3) =
  ({comm_semiring_comm_semiring_0 = (comm_semiring_poly (_A1, _A2, _A3));
     semiring_0_comm_semiring_0 = (semiring_0_poly (_A1, _A2, _A3))}
    : 'a poly comm_semiring_0);;

let rec comm_semiring_0_cancel_poly (_A1, _A2, _A3, _A4) =
  ({comm_semiring_0_comm_semiring_0_cancel =
      (comm_semiring_0_poly
        (_A2, _A3.comm_semiring_0_comm_semiring_0_cancel, _A4));
     semiring_0_cancel_comm_semiring_0_cancel =
       (semiring_0_cancel_poly (_A1, _A2, _A3, _A4))}
    : 'a poly comm_semiring_0_cancel);;

let rec semiring_1_cancel_poly (_A1, _A2, _A3) =
  ({semiring_0_cancel_semiring_1_cancel =
      (semiring_0_cancel_poly
        (_A2.ring_1_comm_ring_1.ring_ring_1.ab_group_add_ring, _A1,
          _A2.comm_ring_comm_ring_1.comm_semiring_0_cancel_comm_ring, _A3));
     semiring_1_semiring_1_cancel =
       (semiring_1_poly
         (_A1, _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel,
           _A3))}
    : 'a poly semiring_1_cancel);;

let rec comm_monoid_mult_poly (_A1, _A2, _A3) =
  ({ab_semigroup_mult_comm_monoid_mult =
      (ab_semigroup_mult_poly (_A1, _A2.comm_semiring_0_comm_semiring_1, _A3));
     monoid_mult_comm_monoid_mult = (monoid_mult_poly (_A1, _A2, _A3));
     dvd_comm_monoid_mult = (dvd_poly (_A1, _A2, _A3))}
    : 'a poly comm_monoid_mult);;

let rec comm_semiring_1_poly (_A1, _A2, _A3) =
  ({comm_monoid_mult_comm_semiring_1 = (comm_monoid_mult_poly (_A1, _A2, _A3));
     comm_semiring_0_comm_semiring_1 =
       (comm_semiring_0_poly (_A1, _A2.comm_semiring_0_comm_semiring_1, _A3));
     semiring_1_comm_semiring_1 = (semiring_1_poly (_A1, _A2, _A3))}
    : 'a poly comm_semiring_1);;

let rec comm_semiring_1_cancel_poly (_A1, _A2, _A3) =
  ({comm_semiring_0_cancel_comm_semiring_1_cancel =
      (comm_semiring_0_cancel_poly
        (_A2.ring_1_comm_ring_1.ring_ring_1.ab_group_add_ring, _A1,
          _A2.comm_ring_comm_ring_1.comm_semiring_0_cancel_comm_ring, _A3));
     comm_semiring_1_comm_semiring_1_cancel =
       (comm_semiring_1_poly
         (_A1, _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel,
           _A3));
     semiring_1_cancel_comm_semiring_1_cancel =
       (semiring_1_cancel_poly (_A1, _A2, _A3))}
    : 'a poly comm_semiring_1_cancel);;

let rec semidom_poly (_A1, _A2) =
  ({comm_semiring_1_cancel_semidom =
      (comm_semiring_1_cancel_poly
        (_A1, _A2.comm_ring_1_idom,
          _A2.semidom_idom.semiring_1_no_zero_divisors_semidom.semiring_no_zero_divisors_semiring_1_no_zero_divisors));
     semiring_1_no_zero_divisors_semidom =
       (semiring_1_no_zero_divisors_poly
         (_A1, _A2.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel,
           _A2.semidom_idom.semiring_1_no_zero_divisors_semidom))}
    : 'a poly semidom);;

let rec divide_poly (_A1, _A2) =
  ({divide = divide_polya (_A1, _A2)} : 'a poly divide);;

let rec semidom_divide_poly (_A1, _A2) =
  ({divide_semidom_divide = (divide_poly (_A1, _A2));
     semidom_semidom_divide = (semidom_poly (_A1, _A2.idom_idom_divide));
     semiring_no_zero_divisors_cancel_semidom_divide =
       (semiring_no_zero_divisors_cancel_poly (_A1, _A2.idom_idom_divide))}
    : 'a poly semidom_divide);;

let rec unit_factor_poly (_A1, _A2, _A3) =
  ({unit_factor = unit_factor_polya (_A1, _A2, _A3)} : 'a poly unit_factor);;

let rec semidom_divide_unit_factor_poly (_A1, _A2, _A3) =
  ({semidom_divide_semidom_divide_unit_factor =
      (semidom_divide_poly (_A1, _A2));
     unit_factor_semidom_divide_unit_factor =
       (unit_factor_poly (_A1, _A2, _A3))}
    : 'a poly semidom_divide_unit_factor);;

let rec algebraic_semidom_poly (_A1, _A2) =
  ({semidom_divide_algebraic_semidom = (semidom_divide_poly (_A1, _A2))} :
    'a poly algebraic_semidom);;

let rec normalization_semidom_poly (_A1, _A2, _A3) =
  ({algebraic_semidom_normalization_semidom =
      (algebraic_semidom_poly (_A1, _A2));
     semidom_divide_unit_factor_normalization_semidom =
       (semidom_divide_unit_factor_poly (_A1, _A2, _A3));
     normalizea = normalize_poly (_A1, _A2, _A3)}
    : 'a poly normalization_semidom);;

let rec factorial_semiring_poly (_A1, _A2, _A3) =
  ({normalization_semidom_factorial_semiring =
      (normalization_semidom_poly
        (_A3, _A1.idom_divide_factorial_ring_gcd,
          _A1.ring_gcd_factorial_ring_gcd.semiring_gcd_ring_gcd.normalization_semidom_semiring_gcd.semidom_divide_unit_factor_normalization_semidom))}
    : 'a poly factorial_semiring);;

let rec comm_monoid_gcd_poly (_A1, _A2, _A3) =
  ({gcd_comm_monoid_gcd = (gcd_polya (_A1, _A2, _A3));
     comm_semiring_1_comm_monoid_gcd =
       (comm_semiring_1_poly
         (_A3, _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel,
           _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.semiring_1_no_zero_divisors_semidom.semiring_no_zero_divisors_semiring_1_no_zero_divisors))}
    : 'a poly comm_monoid_gcd);;

let rec semiring_gcd_poly (_A1, _A2, _A3) =
  ({normalization_semidom_semiring_gcd =
      (normalization_semidom_poly
        (_A3, _A1.idom_divide_factorial_ring_gcd,
          _A1.ring_gcd_factorial_ring_gcd.semiring_gcd_ring_gcd.normalization_semidom_semiring_gcd.semidom_divide_unit_factor_normalization_semidom));
     comm_monoid_gcd_semiring_gcd = (comm_monoid_gcd_poly (_A1, _A2, _A3))}
    : 'a poly semiring_gcd);;

let rec semiring_Gcd_poly (_A1, _A2, _A3) =
  ({gcd_semiring_Gcd = (gcd_poly (_A1, _A2, _A3));
     semiring_gcd_semiring_Gcd = (semiring_gcd_poly (_A1, _A2, _A3))}
    : 'a poly semiring_Gcd);;

let rec factorial_semiring_gcd_poly (_A1, _A2, _A3) =
  ({factorial_semiring_factorial_semiring_gcd =
      (factorial_semiring_poly (_A1, _A2, _A3));
     semiring_Gcd_factorial_semiring_gcd = (semiring_Gcd_poly (_A1, _A2, _A3))}
    : 'a poly factorial_semiring_gcd);;

let rec comm_semiring_1_cancel_crossproduct_poly (_A1, _A2) =
  ({comm_semiring_1_cancel_comm_semiring_1_cancel_crossproduct =
      (comm_semiring_1_cancel_poly
        (_A1, _A2.comm_ring_1_idom,
          _A2.semidom_idom.semiring_1_no_zero_divisors_semidom.semiring_no_zero_divisors_semiring_1_no_zero_divisors))}
    : 'a poly comm_semiring_1_cancel_crossproduct);;

let rec uminus_poly _A = ({uminus = uminus_polya _A} : 'a poly uminus);;

let rec group_add_poly (_A1, _A2) =
  ({cancel_semigroup_add_group_add =
      (cancel_semigroup_add_poly
        (_A1.cancel_comm_monoid_add_ab_group_add, _A2));
     minus_group_add = (minus_poly (_A1, _A2));
     monoid_add_group_add =
       (monoid_add_poly
         (_A1.cancel_comm_monoid_add_ab_group_add.comm_monoid_add_cancel_comm_monoid_add,
           _A2));
     uminus_group_add = (uminus_poly _A1)}
    : 'a poly group_add);;

let rec ab_group_add_poly (_A1, _A2) =
  ({cancel_comm_monoid_add_ab_group_add =
      (cancel_comm_monoid_add_poly (_A1, _A2));
     group_add_ab_group_add = (group_add_poly (_A1, _A2))}
    : 'a poly ab_group_add);;

let rec ring_poly (_A1, _A2, _A3) =
  ({ab_group_add_ring =
      (ab_group_add_poly (_A2.ring_comm_ring.ab_group_add_ring, _A1));
     semiring_0_cancel_ring =
       (semiring_0_cancel_poly
         (_A2.ring_comm_ring.ab_group_add_ring, _A1,
           _A2.comm_semiring_0_cancel_comm_ring, _A3))}
    : 'a poly ring);;

let rec ring_no_zero_divisors_poly (_A1, _A2) =
  ({ring_ring_no_zero_divisors =
      (ring_poly
        (_A1, _A2.comm_ring_1_idom.comm_ring_comm_ring_1,
          _A2.semidom_idom.semiring_1_no_zero_divisors_semidom.semiring_no_zero_divisors_semiring_1_no_zero_divisors));
     semiring_no_zero_divisors_cancel_ring_no_zero_divisors =
       (semiring_no_zero_divisors_cancel_poly (_A1, _A2))}
    : 'a poly ring_no_zero_divisors);;

let rec neg_numeral_poly (_A1, _A2) =
  ({group_add_neg_numeral =
      (group_add_poly
        (_A2.ring_1_comm_ring_1.ring_ring_1.ab_group_add_ring, _A1));
     numeral_neg_numeral =
       (numeral_poly
         (_A1, _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel))}
    : 'a poly neg_numeral);;

let rec ring_1_poly (_A1, _A2, _A3) =
  ({neg_numeral_ring_1 = (neg_numeral_poly (_A1, _A2));
     ring_ring_1 = (ring_poly (_A1, _A2.comm_ring_comm_ring_1, _A3));
     semiring_1_cancel_ring_1 = (semiring_1_cancel_poly (_A1, _A2, _A3))}
    : 'a poly ring_1);;

let rec ring_1_no_zero_divisors_poly (_A1, _A2) =
  ({ring_1_ring_1_no_zero_divisors =
      (ring_1_poly
        (_A1, _A2.comm_ring_1_idom,
          _A2.semidom_idom.semiring_1_no_zero_divisors_semidom.semiring_no_zero_divisors_semiring_1_no_zero_divisors));
     ring_no_zero_divisors_ring_1_no_zero_divisors =
       (ring_no_zero_divisors_poly (_A1, _A2));
     semiring_1_no_zero_divisors_ring_1_no_zero_divisors =
       (semiring_1_no_zero_divisors_poly
         (_A1, _A2.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel,
           _A2.semidom_idom.semiring_1_no_zero_divisors_semidom))}
    : 'a poly ring_1_no_zero_divisors);;

let rec comm_ring_poly (_A1, _A2, _A3) =
  ({comm_semiring_0_cancel_comm_ring =
      (comm_semiring_0_cancel_poly
        (_A2.ring_comm_ring.ab_group_add_ring, _A1,
          _A2.comm_semiring_0_cancel_comm_ring, _A3));
     ring_comm_ring = (ring_poly (_A1, _A2, _A3))}
    : 'a poly comm_ring);;

let rec comm_ring_1_poly (_A1, _A2, _A3) =
  ({comm_ring_comm_ring_1 =
      (comm_ring_poly (_A1, _A2.comm_ring_comm_ring_1, _A3));
     comm_semiring_1_cancel_comm_ring_1 =
       (comm_semiring_1_cancel_poly (_A1, _A2, _A3));
     ring_1_comm_ring_1 = (ring_1_poly (_A1, _A2, _A3))}
    : 'a poly comm_ring_1);;

let rec idom_poly (_A1, _A2) =
  ({comm_ring_1_idom =
      (comm_ring_1_poly
        (_A1, _A2.comm_ring_1_idom,
          _A2.semidom_idom.semiring_1_no_zero_divisors_semidom.semiring_no_zero_divisors_semiring_1_no_zero_divisors));
     ring_1_no_zero_divisors_idom = (ring_1_no_zero_divisors_poly (_A1, _A2));
     semidom_idom = (semidom_poly (_A1, _A2));
     comm_semiring_1_cancel_crossproduct_idom =
       (comm_semiring_1_cancel_crossproduct_poly (_A1, _A2))}
    : 'a poly idom);;

let rec idom_divide_poly (_A1, _A2) =
  ({idom_idom_divide = (idom_poly (_A1, _A2.idom_idom_divide));
     semidom_divide_idom_divide = (semidom_divide_poly (_A1, _A2))}
    : 'a poly idom_divide);;

let rec idom_gcd_poly (_A1, _A2, _A3) =
  ({idom_idom_gcd =
      (idom_poly (_A3, _A1.idom_divide_factorial_ring_gcd.idom_idom_divide));
     comm_monoid_gcd_idom_gcd = (comm_monoid_gcd_poly (_A1, _A2, _A3))}
    : 'a poly idom_gcd);;

let rec ring_gcd_poly (_A1, _A2, _A3) =
  ({semiring_gcd_ring_gcd = (semiring_gcd_poly (_A1, _A2, _A3));
     idom_gcd_ring_gcd = (idom_gcd_poly (_A1, _A2, _A3))}
    : 'a poly ring_gcd);;

let rec factorial_ring_gcd_poly (_A1, _A2, _A3) =
  ({factorial_semiring_gcd_factorial_ring_gcd =
      (factorial_semiring_gcd_poly (_A1, _A2, _A3));
     ring_gcd_factorial_ring_gcd = (ring_gcd_poly (_A1, _A2, _A3));
     idom_divide_factorial_ring_gcd =
       (idom_divide_poly (_A3, _A1.idom_divide_factorial_ring_gcd))}
    : 'a poly factorial_ring_gcd);;

let rec poly_div (_A1, _A2, _A3)
  p q = resultant
          ((factorial_ring_gcd_poly (_A1, _A2, _A3)),
            (equal_poly
              (_A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd,
                _A3)))
          (poly_x_mult_y
            (_A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.comm_monoid_add_semiring_0,
              _A3)
            p)
          (poly_lift
            (_A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd,
              _A3)
            q);;

let rec poly_mult (_A1, _A2, _A3)
  p q = poly_div (_A1, _A2, _A3) p
          (reflect_poly
            (_A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd,
              _A3)
            q);;

let rec mult_1_pos
  (p1, (l1, r1)) (p2, (l2, r2)) =
    select_correct_factor_int_poly (tighten_poly_bounds_binary p1 p2)
      (fun (a, b) ->
        (let (l1a, (r1a, _)) = a in
          (fun (l2a, (r2a, _)) -> (times_rata l1a l2a, times_rata r1a r2a)))
          b)
      ((l1, (r1, sgn_rata
                   (fold_coeffs zero_int
                     (fun a b -> plus_rata (of_int a) (times_rata r1 b)) p1
                     zero_rata))),
        (l2, (r2, sgn_rata
                    (fold_coeffs zero_int
                      (fun a b -> plus_rata (of_int a) (times_rata r2 b)) p2
                      zero_rata))))
      (poly_mult
        (factorial_ring_gcd_int, semiring_gcd_mult_normalize_int, equal_int) p1
        p2);;

let rec mult_1
  x y = (let ((_, (_, r1)), (_, (_, r2))) = (x, y) in
          (if less_rat zero_rata r1
            then (if less_rat zero_rata r2 then mult_1_pos x y
                   else uminus_2 (mult_1_pos x (uminus_1 y)))
            else (if less_rat zero_rata r2
                   then uminus_2 (mult_1_pos (uminus_1 x) y)
                   else mult_1_pos (uminus_1 x) (uminus_1 y))));;

let rec mult_2
  x0 x1 = match x0, x1 with Rational r, Rational q -> Rational (times_rata r q)
    | Rational r, Irrational (n, y) -> mult_rat_1 r y
    | Irrational (n, x), Rational q -> mult_rat_1 q x
    | Irrational (n, x), Irrational (m, y) -> mult_1 x y;;

let rec mult_3
  xb xc = Real_Alg_Invariant (mult_2 (rep_real_alg_3 xb) (rep_real_alg_3 xc));;

let rec times_real_alga
  (Real_Alg_Quotient xa) (Real_Alg_Quotient x) =
    Real_Alg_Quotient (mult_3 xa x);;

let rec times_reala (Real_of x) (Real_of y) = Real_of (times_real_alga x y);;

let times_real = ({times = times_reala} : real times);;

let dvd_real = ({times_dvd = times_real} : real dvd);;

let rec uminus_3 xa = Real_Alg_Invariant (uminus_2 (rep_real_alg_3 xa));;

let rec uminus_real_alga
  (Real_Alg_Quotient x) = Real_Alg_Quotient (uminus_3 x);;

let rec of_rat_3 xa = Real_Alg_Invariant (Rational xa);;

let rec of_rat_real_alg x = Real_Alg_Quotient (of_rat_3 x);;

let zero_real_alga : real_alg = of_rat_real_alg zero_rata;;

let zero_reala : real = Real_of zero_real_alga;;

let rec compare_rat x = comparator_of (equal_rat, linorder_rat) x;;

let rec compare_rat_1
  x (p, (l, r)) =
    (if less_rat x l then Lt
      else (if less_rat r x then Gt
             else (if equal_rata
                        (sgn_rata
                          (fold_coeffs zero_int
                            (fun a b -> plus_rata (of_int a) (times_rata x b)) p
                            zero_rata))
                        (sgn_rata
                          (fold_coeffs zero_int
                            (fun a b -> plus_rata (of_int a) (times_rata r b)) p
                            zero_rata))
                    then Gt else Lt)));;

let rec compare_1
  p1 p2 l1 r1 sr1 l2 r2 sr2 =
    (if less_rat r1 l2 then Lt
      else (if less_rat r2 l1 then Gt
             else (let (l1a, (r1a, sr1a)) = tighten_poly_bounds p1 l1 r1 sr1 in
                   let (l2a, (a, b)) = tighten_poly_bounds p2 l2 r2 sr2 in
                    compare_1 p1 p2 l1a r1a sr1a l2a a b)));;

let rec invert_order = function Lt -> Gt
                       | Gt -> Lt
                       | Eq -> Eq;;

let rec compare_2
  x0 x1 = match x0, x1 with Rational r, Rational q -> compare_rat r q
    | Irrational (n, (p, (la, ra))), Irrational (m, (q, (l, r))) ->
        (if equal_polya (zero_int, equal_int) p q && equal_nata n m then Eq
          else compare_1 p q la ra
                 (sgn_rata
                   (fold_coeffs zero_int
                     (fun a b -> plus_rata (of_int a) (times_rata ra b)) p
                     zero_rata))
                 l r (sgn_rata
                       (fold_coeffs zero_int
                         (fun a b -> plus_rata (of_int a) (times_rata r b)) q
                         zero_rata)))
    | Rational r, Irrational (uu, xx) -> compare_rat_1 r xx
    | Irrational (uv, xx), Rational r -> invert_order (compare_rat_1 r xx);;

let rec compare_3 xa xc = compare_2 (rep_real_alg_3 xa) (rep_real_alg_3 xc);;

let rec compare_real_alg
  (Real_Alg_Quotient xc) (Real_Alg_Quotient xa) = compare_3 xc xa;;

let rec less_real_alg x = lt_of_comp compare_real_alg x;;

let rec less_real (Real_of x) (Real_of y) = less_real_alg x y;;

let rec abs_real_alga
  x = (if less_real (Real_of x) zero_reala then uminus_real_alga x else x);;

let rec abs_reala (Real_of x) = Real_of (abs_real_alga x);;

let abs_real = ({abs = abs_reala} : real abs);;

let one_real_alga : real_alg = of_rat_real_alg one_rata;;

let one_reala : real = Real_of one_real_alga;;

let one_real = ({one = one_reala} : real one);;

let rec rai_ub (uu, (uv, r)) = r;;

let rec sgn_1 x = sgn_rata (rai_ub x);;

let rec sgn_2 = function Rational r -> sgn_rata r
                | Irrational (n, rai) -> sgn_1 rai;;

let rec sgn_3 xa = sgn_2 (rep_real_alg_3 xa);;

let rec sgn_real_alg_rat (Real_Alg_Quotient xa) = sgn_3 xa;;

let rec sgn_real_alga x = of_rat_real_alg (sgn_real_alg_rat x);;

let rec sgn_reala (Real_of x) = Real_of (sgn_real_alga x);;

let sgn_real = ({sgn = sgn_reala} : real sgn);;

let rec uminus_reala (Real_of x) = Real_of (uminus_real_alga x);;

let rec pcompose (_A1, _A2, _A3)
  p q = fold_coeffs
          _A2.semiring_0_comm_semiring_0.mult_zero_semiring_0.zero_mult_zero
          (fun a c ->
            plus_polya
              (_A2.semiring_0_comm_semiring_0.comm_monoid_add_semiring_0, _A1)
              (pCons
                (_A2.semiring_0_comm_semiring_0.mult_zero_semiring_0.zero_mult_zero,
                  _A1)
                a (zero_polya
                    _A2.semiring_0_comm_semiring_0.mult_zero_semiring_0.zero_mult_zero))
              (times_polya (_A1, _A2, _A3) q c))
          p (zero_polya
              _A2.semiring_0_comm_semiring_0.mult_zero_semiring_0.zero_mult_zero);;

let rec poly_add_rat
  r p = (let (n, d) = quotient_of r in
         let pa =
           (let fs = coeffs zero_int p in
            let k = size_list fs in
             poly_of_list (comm_monoid_add_int, equal_int)
               (map (fun (fi, i) ->
                      times_inta fi
                        (binary_power monoid_mult_int d (minus_nata k (suc i))))
                 (zip fs (upt zero_nata k))))
           in
         let pb =
           pcompose
             (equal_int, comm_semiring_0_int, semiring_no_zero_divisors_int) pa
             (pCons (zero_int, equal_int) (uminus_inta n)
               (pCons (zero_int, equal_int) d (zero_polya zero_int)))
           in
          pb);;

let rec add_rat_1
  r1 (p2, (l2, r2)) =
    (let p = cf_pos_poly (poly_add_rat r1 p2) in
     let (l, (r, _)) =
       tighten_poly_bounds_for_x p zero_rata (plus_rata l2 r1) (plus_rata r2 r1)
         (sgn_rata
           (fold_coeffs zero_int
             (fun a b -> plus_rata (of_int a) (times_rata (plus_rata r2 r1) b))
             p zero_rata))
       in
      (p, (l, r)));;

let rec x_y (_A1, _A2, _A3)
  = pCons ((zero_poly
             _A1.group_add_ab_group_add.monoid_add_group_add.zero_monoid_add),
            (equal_poly
              (_A1.group_add_ab_group_add.monoid_add_group_add.zero_monoid_add,
                _A2)))
      (pCons
        (_A1.group_add_ab_group_add.monoid_add_group_add.zero_monoid_add, _A2)
        (zero _A1.group_add_ab_group_add.monoid_add_group_add.zero_monoid_add)
        (pCons
          (_A1.group_add_ab_group_add.monoid_add_group_add.zero_monoid_add, _A2)
          (one _A3.semiring_1_comm_semiring_1.semiring_numeral_semiring_1.numeral_semiring_numeral.one_numeral)
          (zero_polya
            _A1.group_add_ab_group_add.monoid_add_group_add.zero_monoid_add)))
      (pCons
        ((zero_poly
           _A1.group_add_ab_group_add.monoid_add_group_add.zero_monoid_add),
          (equal_poly
            (_A1.group_add_ab_group_add.monoid_add_group_add.zero_monoid_add,
              _A2)))
        (uminus_polya _A1 (one_polya _A3))
        (zero_polya
          (zero_poly
            _A1.group_add_ab_group_add.monoid_add_group_add.zero_monoid_add)));;

let rec poly_x_minus_y (_A1, _A2, _A3, _A4)
  p = pcompose
        ((equal_poly
           (_A3.comm_semiring_0_comm_semiring_1.semiring_0_comm_semiring_0.mult_zero_semiring_0.zero_mult_zero,
             _A2)),
          (comm_semiring_0_poly
            (_A2, _A3.comm_semiring_0_comm_semiring_1, _A4)),
          (semiring_no_zero_divisors_poly
            (_A2, _A3.comm_semiring_0_comm_semiring_1, _A4)))
        (poly_lift
          (_A1.group_add_ab_group_add.monoid_add_group_add.zero_monoid_add, _A2)
          p)
        (x_y (_A1, _A2, _A3));;

let rec poly_add (_A1, _A2, _A3)
  p q = resultant
          ((factorial_ring_gcd_poly (_A1, _A2, _A3)),
            (equal_poly
              (_A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd,
                _A3)))
          (poly_x_minus_y
            (_A1.idom_divide_factorial_ring_gcd.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.ring_ring_1.ab_group_add_ring,
              _A3,
              _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel,
              _A1.idom_divide_factorial_ring_gcd.idom_idom_divide.semidom_idom.semiring_1_no_zero_divisors_semidom.semiring_no_zero_divisors_semiring_1_no_zero_divisors)
            p)
          (poly_lift
            (_A1.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd,
              _A3)
            q);;

let rec add_1
  (p1, (l1, r1)) (p2, (l2, r2)) =
    select_correct_factor_int_poly (tighten_poly_bounds_binary p1 p2)
      (fun (a, b) ->
        (let (l1a, (r1a, _)) = a in
          (fun (l2a, (r2a, _)) -> (plus_rata l1a l2a, plus_rata r1a r2a)))
          b)
      ((l1, (r1, sgn_rata
                   (fold_coeffs zero_int
                     (fun a b -> plus_rata (of_int a) (times_rata r1 b)) p1
                     zero_rata))),
        (l2, (r2, sgn_rata
                    (fold_coeffs zero_int
                      (fun a b -> plus_rata (of_int a) (times_rata r2 b)) p2
                      zero_rata))))
      (poly_add
        (factorial_ring_gcd_int, semiring_gcd_mult_normalize_int, equal_int) p1
        p2);;

let rec add_2
  x0 x1 = match x0, x1 with Rational r, Rational q -> Rational (plus_rata r q)
    | Rational r, Irrational (n, x) -> Irrational (n, add_rat_1 r x)
    | Irrational (n, x), Rational q -> Irrational (n, add_rat_1 q x)
    | Irrational (n, x), Irrational (m, y) -> add_1 x y;;

let rec add_3
  xb xc = Real_Alg_Invariant (add_2 (rep_real_alg_3 xb) (rep_real_alg_3 xc));;

let rec plus_real_alga
  (Real_Alg_Quotient xa) (Real_Alg_Quotient x) =
    Real_Alg_Quotient (add_3 xa x);;

let rec minus_real_alga x y = plus_real_alga x (uminus_real_alga y);;

let rec minus_reala (Real_of x) (Real_of y) = Real_of (minus_real_alga x y);;

let rec plus_reala (Real_of x) (Real_of y) = Real_of (plus_real_alga x y);;

let plus_real = ({plus = plus_reala} : real plus);;

let semigroup_add_real =
  ({plus_semigroup_add = plus_real} : real semigroup_add);;

let cancel_semigroup_add_real =
  ({semigroup_add_cancel_semigroup_add = semigroup_add_real} :
    real cancel_semigroup_add);;

let ab_semigroup_add_real =
  ({semigroup_add_ab_semigroup_add = semigroup_add_real} :
    real ab_semigroup_add);;

let minus_real = ({minus = minus_reala} : real minus);;

let cancel_ab_semigroup_add_real =
  ({ab_semigroup_add_cancel_ab_semigroup_add = ab_semigroup_add_real;
     cancel_semigroup_add_cancel_ab_semigroup_add = cancel_semigroup_add_real;
     minus_cancel_ab_semigroup_add = minus_real}
    : real cancel_ab_semigroup_add);;

let zero_real = ({zero = zero_reala} : real zero);;

let monoid_add_real =
  ({semigroup_add_monoid_add = semigroup_add_real; zero_monoid_add = zero_real}
    : real monoid_add);;

let comm_monoid_add_real =
  ({ab_semigroup_add_comm_monoid_add = ab_semigroup_add_real;
     monoid_add_comm_monoid_add = monoid_add_real}
    : real comm_monoid_add);;

let cancel_comm_monoid_add_real =
  ({cancel_ab_semigroup_add_cancel_comm_monoid_add =
      cancel_ab_semigroup_add_real;
     comm_monoid_add_cancel_comm_monoid_add = comm_monoid_add_real}
    : real cancel_comm_monoid_add);;

let mult_zero_real =
  ({times_mult_zero = times_real; zero_mult_zero = zero_real} :
    real mult_zero);;

let semigroup_mult_real =
  ({times_semigroup_mult = times_real} : real semigroup_mult);;

let semiring_real =
  ({ab_semigroup_add_semiring = ab_semigroup_add_real;
     semigroup_mult_semiring = semigroup_mult_real}
    : real semiring);;

let semiring_0_real =
  ({comm_monoid_add_semiring_0 = comm_monoid_add_real;
     mult_zero_semiring_0 = mult_zero_real; semiring_semiring_0 = semiring_real}
    : real semiring_0);;

let semiring_0_cancel_real =
  ({cancel_comm_monoid_add_semiring_0_cancel = cancel_comm_monoid_add_real;
     semiring_0_semiring_0_cancel = semiring_0_real}
    : real semiring_0_cancel);;

let ab_semigroup_mult_real =
  ({semigroup_mult_ab_semigroup_mult = semigroup_mult_real} :
    real ab_semigroup_mult);;

let comm_semiring_real =
  ({ab_semigroup_mult_comm_semiring = ab_semigroup_mult_real;
     semiring_comm_semiring = semiring_real}
    : real comm_semiring);;

let comm_semiring_0_real =
  ({comm_semiring_comm_semiring_0 = comm_semiring_real;
     semiring_0_comm_semiring_0 = semiring_0_real}
    : real comm_semiring_0);;

let comm_semiring_0_cancel_real =
  ({comm_semiring_0_comm_semiring_0_cancel = comm_semiring_0_real;
     semiring_0_cancel_comm_semiring_0_cancel = semiring_0_cancel_real}
    : real comm_semiring_0_cancel);;

let power_real =
  ({one_power = one_real; times_power = times_real} : real power);;

let monoid_mult_real =
  ({semigroup_mult_monoid_mult = semigroup_mult_real;
     power_monoid_mult = power_real}
    : real monoid_mult);;

let numeral_real =
  ({one_numeral = one_real; semigroup_add_numeral = semigroup_add_real} :
    real numeral);;

let semiring_numeral_real =
  ({monoid_mult_semiring_numeral = monoid_mult_real;
     numeral_semiring_numeral = numeral_real;
     semiring_semiring_numeral = semiring_real}
    : real semiring_numeral);;

let zero_neq_one_real =
  ({one_zero_neq_one = one_real; zero_zero_neq_one = zero_real} :
    real zero_neq_one);;

let semiring_1_real =
  ({semiring_numeral_semiring_1 = semiring_numeral_real;
     semiring_0_semiring_1 = semiring_0_real;
     zero_neq_one_semiring_1 = zero_neq_one_real}
    : real semiring_1);;

let semiring_1_cancel_real =
  ({semiring_0_cancel_semiring_1_cancel = semiring_0_cancel_real;
     semiring_1_semiring_1_cancel = semiring_1_real}
    : real semiring_1_cancel);;

let comm_monoid_mult_real =
  ({ab_semigroup_mult_comm_monoid_mult = ab_semigroup_mult_real;
     monoid_mult_comm_monoid_mult = monoid_mult_real;
     dvd_comm_monoid_mult = dvd_real}
    : real comm_monoid_mult);;

let comm_semiring_1_real =
  ({comm_monoid_mult_comm_semiring_1 = comm_monoid_mult_real;
     comm_semiring_0_comm_semiring_1 = comm_semiring_0_real;
     semiring_1_comm_semiring_1 = semiring_1_real}
    : real comm_semiring_1);;

let comm_semiring_1_cancel_real =
  ({comm_semiring_0_cancel_comm_semiring_1_cancel = comm_semiring_0_cancel_real;
     comm_semiring_1_comm_semiring_1_cancel = comm_semiring_1_real;
     semiring_1_cancel_comm_semiring_1_cancel = semiring_1_cancel_real}
    : real comm_semiring_1_cancel);;

let comm_semiring_1_cancel_crossproduct_real =
  ({comm_semiring_1_cancel_comm_semiring_1_cancel_crossproduct =
      comm_semiring_1_cancel_real}
    : real comm_semiring_1_cancel_crossproduct);;

let semiring_no_zero_divisors_real =
  ({semiring_0_semiring_no_zero_divisors = semiring_0_real} :
    real semiring_no_zero_divisors);;

let semiring_1_no_zero_divisors_real =
  ({semiring_1_semiring_1_no_zero_divisors = semiring_1_real;
     semiring_no_zero_divisors_semiring_1_no_zero_divisors =
       semiring_no_zero_divisors_real}
    : real semiring_1_no_zero_divisors);;

let semiring_no_zero_divisors_cancel_real =
  ({semiring_no_zero_divisors_semiring_no_zero_divisors_cancel =
      semiring_no_zero_divisors_real}
    : real semiring_no_zero_divisors_cancel);;

let uminus_real = ({uminus = uminus_reala} : real uminus);;

let group_add_real =
  ({cancel_semigroup_add_group_add = cancel_semigroup_add_real;
     minus_group_add = minus_real; monoid_add_group_add = monoid_add_real;
     uminus_group_add = uminus_real}
    : real group_add);;

let ab_group_add_real =
  ({cancel_comm_monoid_add_ab_group_add = cancel_comm_monoid_add_real;
     group_add_ab_group_add = group_add_real}
    : real ab_group_add);;

let ring_real =
  ({ab_group_add_ring = ab_group_add_real;
     semiring_0_cancel_ring = semiring_0_cancel_real}
    : real ring);;

let ring_no_zero_divisors_real =
  ({ring_ring_no_zero_divisors = ring_real;
     semiring_no_zero_divisors_cancel_ring_no_zero_divisors =
       semiring_no_zero_divisors_cancel_real}
    : real ring_no_zero_divisors);;

let neg_numeral_real =
  ({group_add_neg_numeral = group_add_real; numeral_neg_numeral = numeral_real}
    : real neg_numeral);;

let ring_1_real =
  ({neg_numeral_ring_1 = neg_numeral_real; ring_ring_1 = ring_real;
     semiring_1_cancel_ring_1 = semiring_1_cancel_real}
    : real ring_1);;

let ring_1_no_zero_divisors_real =
  ({ring_1_ring_1_no_zero_divisors = ring_1_real;
     ring_no_zero_divisors_ring_1_no_zero_divisors = ring_no_zero_divisors_real;
     semiring_1_no_zero_divisors_ring_1_no_zero_divisors =
       semiring_1_no_zero_divisors_real}
    : real ring_1_no_zero_divisors);;

let comm_ring_real =
  ({comm_semiring_0_cancel_comm_ring = comm_semiring_0_cancel_real;
     ring_comm_ring = ring_real}
    : real comm_ring);;

let comm_ring_1_real =
  ({comm_ring_comm_ring_1 = comm_ring_real;
     comm_semiring_1_cancel_comm_ring_1 = comm_semiring_1_cancel_real;
     ring_1_comm_ring_1 = ring_1_real}
    : real comm_ring_1);;

let semidom_real =
  ({comm_semiring_1_cancel_semidom = comm_semiring_1_cancel_real;
     semiring_1_no_zero_divisors_semidom = semiring_1_no_zero_divisors_real}
    : real semidom);;

let idom_real =
  ({comm_ring_1_idom = comm_ring_1_real;
     ring_1_no_zero_divisors_idom = ring_1_no_zero_divisors_real;
     semidom_idom = semidom_real;
     comm_semiring_1_cancel_crossproduct_idom =
       comm_semiring_1_cancel_crossproduct_real}
    : real idom);;

let rec inverse_1
  (p, (l, r)) =
    real_alg_2
      (abs_int_poly (reflect_poly (zero_int, equal_int) p),
        (inverse_rata r, inverse_rata l));;

let rec inverse_2 = function Rational r -> Rational (inverse_rata r)
                    | Irrational (n, x) -> inverse_1 x;;

let rec inverse_3 xa = Real_Alg_Invariant (inverse_2 (rep_real_alg_3 xa));;

let rec inverse_real_alga
  (Real_Alg_Quotient x) = Real_Alg_Quotient (inverse_3 x);;

let rec inverse_reala (Real_of x) = Real_of (inverse_real_alga x);;

let rec divide_real_alga x y = times_real_alga x (inverse_real_alga y);;

let rec divide_reala (Real_of x) (Real_of y) = Real_of (divide_real_alga x y);;

let ufd_real = ({idom_ufd = idom_real} : real ufd);;

let divide_real = ({divide = divide_reala} : real divide);;

let inverse_real =
  ({divide_inverse = divide_real; inverse = inverse_reala} : real inverse);;

let division_ring_real =
  ({inverse_division_ring = inverse_real;
     ring_1_no_zero_divisors_division_ring = ring_1_no_zero_divisors_real}
    : real division_ring);;

let semidom_divide_real =
  ({divide_semidom_divide = divide_real; semidom_semidom_divide = semidom_real;
     semiring_no_zero_divisors_cancel_semidom_divide =
       semiring_no_zero_divisors_cancel_real}
    : real semidom_divide);;

let idom_divide_real =
  ({idom_idom_divide = idom_real;
     semidom_divide_idom_divide = semidom_divide_real}
    : real idom_divide);;

let field_real =
  ({division_ring_field = division_ring_real;
     idom_divide_field = idom_divide_real; ufd_field = ufd_real}
    : real field);;

let rec less_eq_real_alg x = le_of_comp compare_real_alg x;;

let rec less_eq_real (Real_of x) (Real_of y) = less_eq_real_alg x y;;

let ord_real = ({less_eq = less_eq_real; less = less_real} : real ord);;

let abs_if_real =
  ({abs_abs_if = abs_real; minus_abs_if = minus_real;
     uminus_abs_if = uminus_real; zero_abs_if = zero_real;
     ord_abs_if = ord_real}
    : real abs_if);;

let semiring_char_0_real =
  ({semiring_1_semiring_char_0 = semiring_1_real} : real semiring_char_0);;

let ring_char_0_real =
  ({semiring_char_0_ring_char_0 = semiring_char_0_real;
     ring_1_ring_char_0 = ring_1_real}
    : real ring_char_0);;

let preorder_real = ({ord_preorder = ord_real} : real preorder);;

let order_real = ({preorder_order = preorder_real} : real order);;

let no_bot_real = ({order_no_bot = order_real} : real no_bot);;

let no_top_real = ({order_no_top = order_real} : real no_top);;

let linorder_real = ({order_linorder = order_real} : real linorder);;

let idom_abs_sgn_real =
  ({abs_idom_abs_sgn = abs_real; sgn_idom_abs_sgn = sgn_real;
     idom_idom_abs_sgn = idom_real}
    : real idom_abs_sgn);;

let ordered_ab_semigroup_add_real =
  ({ab_semigroup_add_ordered_ab_semigroup_add = ab_semigroup_add_real;
     order_ordered_ab_semigroup_add = order_real}
    : real ordered_ab_semigroup_add);;

let strict_ordered_ab_semigroup_add_real =
  ({ordered_ab_semigroup_add_strict_ordered_ab_semigroup_add =
      ordered_ab_semigroup_add_real}
    : real strict_ordered_ab_semigroup_add);;

let ordered_cancel_ab_semigroup_add_real =
  ({cancel_ab_semigroup_add_ordered_cancel_ab_semigroup_add =
      cancel_ab_semigroup_add_real;
     strict_ordered_ab_semigroup_add_ordered_cancel_ab_semigroup_add =
       strict_ordered_ab_semigroup_add_real}
    : real ordered_cancel_ab_semigroup_add);;

let ordered_comm_monoid_add_real =
  ({comm_monoid_add_ordered_comm_monoid_add = comm_monoid_add_real;
     ordered_ab_semigroup_add_ordered_comm_monoid_add =
       ordered_ab_semigroup_add_real}
    : real ordered_comm_monoid_add);;

let ordered_semiring_real =
  ({ordered_comm_monoid_add_ordered_semiring = ordered_comm_monoid_add_real;
     semiring_ordered_semiring = semiring_real}
    : real ordered_semiring);;

let ordered_semiring_0_real =
  ({ordered_semiring_ordered_semiring_0 = ordered_semiring_real;
     semiring_0_ordered_semiring_0 = semiring_0_real}
    : real ordered_semiring_0);;

let ordered_cancel_semiring_real =
  ({ordered_cancel_ab_semigroup_add_ordered_cancel_semiring =
      ordered_cancel_ab_semigroup_add_real;
     ordered_semiring_0_ordered_cancel_semiring = ordered_semiring_0_real;
     semiring_0_cancel_ordered_cancel_semiring = semiring_0_cancel_real}
    : real ordered_cancel_semiring);;

let ordered_ab_semigroup_add_imp_le_real =
  ({ordered_cancel_ab_semigroup_add_ordered_ab_semigroup_add_imp_le =
      ordered_cancel_ab_semigroup_add_real}
    : real ordered_ab_semigroup_add_imp_le);;

let strict_ordered_comm_monoid_add_real =
  ({comm_monoid_add_strict_ordered_comm_monoid_add = comm_monoid_add_real;
     strict_ordered_ab_semigroup_add_strict_ordered_comm_monoid_add =
       strict_ordered_ab_semigroup_add_real}
    : real strict_ordered_comm_monoid_add);;

let ordered_cancel_comm_monoid_add_real =
  ({ordered_cancel_ab_semigroup_add_ordered_cancel_comm_monoid_add =
      ordered_cancel_ab_semigroup_add_real;
     ordered_comm_monoid_add_ordered_cancel_comm_monoid_add =
       ordered_comm_monoid_add_real;
     strict_ordered_comm_monoid_add_ordered_cancel_comm_monoid_add =
       strict_ordered_comm_monoid_add_real}
    : real ordered_cancel_comm_monoid_add);;

let ordered_ab_semigroup_monoid_add_imp_le_real =
  ({cancel_comm_monoid_add_ordered_ab_semigroup_monoid_add_imp_le =
      cancel_comm_monoid_add_real;
     ordered_ab_semigroup_add_imp_le_ordered_ab_semigroup_monoid_add_imp_le =
       ordered_ab_semigroup_add_imp_le_real;
     ordered_cancel_comm_monoid_add_ordered_ab_semigroup_monoid_add_imp_le =
       ordered_cancel_comm_monoid_add_real}
    : real ordered_ab_semigroup_monoid_add_imp_le);;

let ordered_ab_group_add_real =
  ({ab_group_add_ordered_ab_group_add = ab_group_add_real;
     ordered_ab_semigroup_monoid_add_imp_le_ordered_ab_group_add =
       ordered_ab_semigroup_monoid_add_imp_le_real}
    : real ordered_ab_group_add);;

let ordered_ring_real =
  ({ordered_ab_group_add_ordered_ring = ordered_ab_group_add_real;
     ordered_cancel_semiring_ordered_ring = ordered_cancel_semiring_real;
     ring_ordered_ring = ring_real}
    : real ordered_ring);;

let field_char_0_real =
  ({field_field_char_0 = field_real;
     ring_char_0_field_char_0 = ring_char_0_real}
    : real field_char_0);;

let zero_less_one_real =
  ({order_zero_less_one = order_real;
     zero_neq_one_zero_less_one = zero_neq_one_real}
    : real zero_less_one);;

let field_abs_sgn_real =
  ({field_field_abs_sgn = field_real;
     idom_abs_sgn_field_abs_sgn = idom_abs_sgn_real}
    : real field_abs_sgn);;

let dense_order_real = ({order_dense_order = order_real} : real dense_order);;

let linordered_ab_semigroup_add_real =
  ({ordered_ab_semigroup_add_linordered_ab_semigroup_add =
      ordered_ab_semigroup_add_real;
     linorder_linordered_ab_semigroup_add = linorder_real}
    : real linordered_ab_semigroup_add);;

let linordered_cancel_ab_semigroup_add_real =
  ({linordered_ab_semigroup_add_linordered_cancel_ab_semigroup_add =
      linordered_ab_semigroup_add_real;
     ordered_ab_semigroup_add_imp_le_linordered_cancel_ab_semigroup_add =
       ordered_ab_semigroup_add_imp_le_real}
    : real linordered_cancel_ab_semigroup_add);;

let linordered_semiring_real =
  ({linordered_cancel_ab_semigroup_add_linordered_semiring =
      linordered_cancel_ab_semigroup_add_real;
     ordered_ab_semigroup_monoid_add_imp_le_linordered_semiring =
       ordered_ab_semigroup_monoid_add_imp_le_real;
     ordered_cancel_semiring_linordered_semiring = ordered_cancel_semiring_real}
    : real linordered_semiring);;

let linordered_semiring_strict_real =
  ({linordered_semiring_linordered_semiring_strict = linordered_semiring_real} :
    real linordered_semiring_strict);;

let linordered_semiring_1_real =
  ({linordered_semiring_linordered_semiring_1 = linordered_semiring_real;
     semiring_1_linordered_semiring_1 = semiring_1_real;
     zero_less_one_linordered_semiring_1 = zero_less_one_real}
    : real linordered_semiring_1);;

let linordered_semiring_1_strict_real =
  ({linordered_semiring_1_linordered_semiring_1_strict =
      linordered_semiring_1_real;
     linordered_semiring_strict_linordered_semiring_1_strict =
       linordered_semiring_strict_real}
    : real linordered_semiring_1_strict);;

let ordered_ab_group_add_abs_real =
  ({abs_ordered_ab_group_add_abs = abs_real;
     ordered_ab_group_add_ordered_ab_group_add_abs = ordered_ab_group_add_real}
    : real ordered_ab_group_add_abs);;

let linordered_ab_group_add_real =
  ({linordered_cancel_ab_semigroup_add_linordered_ab_group_add =
      linordered_cancel_ab_semigroup_add_real;
     ordered_ab_group_add_linordered_ab_group_add = ordered_ab_group_add_real}
    : real linordered_ab_group_add);;

let linordered_ring_real =
  ({linordered_ab_group_add_linordered_ring = linordered_ab_group_add_real;
     ordered_ab_group_add_abs_linordered_ring = ordered_ab_group_add_abs_real;
     abs_if_linordered_ring = abs_if_real;
     linordered_semiring_linordered_ring = linordered_semiring_real;
     ordered_ring_linordered_ring = ordered_ring_real}
    : real linordered_ring);;

let linordered_ring_strict_real =
  ({linordered_ring_linordered_ring_strict = linordered_ring_real;
     linordered_semiring_strict_linordered_ring_strict =
       linordered_semiring_strict_real;
     ring_no_zero_divisors_linordered_ring_strict = ring_no_zero_divisors_real}
    : real linordered_ring_strict);;

let ordered_comm_semiring_real =
  ({comm_semiring_0_ordered_comm_semiring = comm_semiring_0_real;
     ordered_semiring_ordered_comm_semiring = ordered_semiring_real}
    : real ordered_comm_semiring);;

let ordered_cancel_comm_semiring_real =
  ({comm_semiring_0_cancel_ordered_cancel_comm_semiring =
      comm_semiring_0_cancel_real;
     ordered_cancel_semiring_ordered_cancel_comm_semiring =
       ordered_cancel_semiring_real;
     ordered_comm_semiring_ordered_cancel_comm_semiring =
       ordered_comm_semiring_real}
    : real ordered_cancel_comm_semiring);;

let linordered_comm_semiring_strict_real =
  ({linordered_semiring_strict_linordered_comm_semiring_strict =
      linordered_semiring_strict_real;
     ordered_cancel_comm_semiring_linordered_comm_semiring_strict =
       ordered_cancel_comm_semiring_real}
    : real linordered_comm_semiring_strict);;

let linordered_nonzero_semiring_real =
  ({semiring_char_0_linordered_nonzero_semiring = semiring_char_0_real;
     linorder_linordered_nonzero_semiring = linorder_real;
     comm_semiring_1_linordered_nonzero_semiring = comm_semiring_1_real;
     ordered_comm_semiring_linordered_nonzero_semiring =
       ordered_comm_semiring_real;
     zero_less_one_linordered_nonzero_semiring = zero_less_one_real}
    : real linordered_nonzero_semiring);;

let linordered_semidom_real =
  ({linordered_comm_semiring_strict_linordered_semidom =
      linordered_comm_semiring_strict_real;
     linordered_nonzero_semiring_linordered_semidom =
       linordered_nonzero_semiring_real;
     semidom_linordered_semidom = semidom_real}
    : real linordered_semidom);;

let ordered_comm_ring_real =
  ({comm_ring_ordered_comm_ring = comm_ring_real;
     ordered_cancel_comm_semiring_ordered_comm_ring =
       ordered_cancel_comm_semiring_real;
     ordered_ring_ordered_comm_ring = ordered_ring_real}
    : real ordered_comm_ring);;

let ordered_ring_abs_real =
  ({ordered_ab_group_add_abs_ordered_ring_abs = ordered_ab_group_add_abs_real;
     ordered_ring_ordered_ring_abs = ordered_ring_real}
    : real ordered_ring_abs);;

let linordered_idom_real =
  ({ring_char_0_linordered_idom = ring_char_0_real;
     idom_abs_sgn_linordered_idom = idom_abs_sgn_real;
     linordered_ring_strict_linordered_idom = linordered_ring_strict_real;
     linordered_semidom_linordered_idom = linordered_semidom_real;
     linordered_semiring_1_strict_linordered_idom =
       linordered_semiring_1_strict_real;
     ordered_comm_ring_linordered_idom = ordered_comm_ring_real;
     ordered_ring_abs_linordered_idom = ordered_ring_abs_real}
    : real linordered_idom);;

let non_strict_order_real =
  ({ord_non_strict_order = ord_real} : real non_strict_order);;

let ordered_ab_semigroup_real =
  ({ab_semigroup_add_ordered_ab_semigroup = ab_semigroup_add_real;
     monoid_add_ordered_ab_semigroup = monoid_add_real;
     non_strict_order_ordered_ab_semigroup = non_strict_order_real}
    : real ordered_ab_semigroup);;

let ordered_semiring_0_reala =
  ({semiring_0_ordered_semiring_0a = semiring_0_real;
     ordered_ab_semigroup_ordered_semiring_0 = ordered_ab_semigroup_real}
    : real ordered_semiring_0a);;

let ordered_semiring_1_real =
  ({semiring_1_ordered_semiring_1 = semiring_1_real;
     ordered_semiring_0_ordered_semiring_1 = ordered_semiring_0_reala}
    : real ordered_semiring_1);;

let poly_carrier_real =
  ({comm_semiring_1_poly_carrier = comm_semiring_1_real;
     ordered_semiring_1_poly_carrier = ordered_semiring_1_real}
    : real poly_carrier);;

let dense_linorder_real =
  ({dense_order_dense_linorder = dense_order_real;
     linorder_dense_linorder = linorder_real}
    : real dense_linorder);;

let unbounded_dense_linorder_real =
  ({dense_linorder_unbounded_dense_linorder = dense_linorder_real;
     no_bot_unbounded_dense_linorder = no_bot_real;
     no_top_unbounded_dense_linorder = no_top_real}
    : real unbounded_dense_linorder);;

let linordered_field_real =
  ({field_abs_sgn_linordered_field = field_abs_sgn_real;
     field_char_0_linordered_field = field_char_0_real;
     unbounded_dense_linorder_linordered_field = unbounded_dense_linorder_real;
     linordered_idom_linordered_field = linordered_idom_real}
    : real linordered_field);;

let rec floor_1
  (p, (l, r)) =
    (let (la, (ra, sr)) =
       tighten_poly_bounds_epsilon p
         (divide_rata one_rata (of_int (Int_of_integer (Z.of_int 2)))) l r
         (sgn_rata
           (fold_coeffs zero_int
             (fun a b -> plus_rata (of_int a) (times_rata r b)) p zero_rata))
       in
     let fr = floor_rat ra in
     let fl = floor_rat la in
     let fra = of_int fr in
      (if equal_inta fr fl then fr
        else (let (lb, (_, _)) = tighten_poly_bounds_for_x p fra la ra sr in
               (if less_rat fra lb then fr else fl))));;

let rec floor_2 = function Rational r -> floor_rat r
                  | Irrational (n, rai) -> floor_1 rai;;

let rec floor_3 xa = floor_2 (rep_real_alg_3 xa);;

let rec floor_real_alg (Real_Alg_Quotient xa) = floor_3 xa;;

let rec floor_real (Real_of x) = floor_real_alg x;;

let archimedean_field_real =
  ({linordered_field_archimedean_field = linordered_field_real} :
    real archimedean_field);;

let large_ordered_semiring_1_real =
  ({poly_carrier_large_ordered_semiring_1 = poly_carrier_real} :
    real large_ordered_semiring_1);;

let floor_ceiling_real =
  ({archimedean_field_floor_ceiling = archimedean_field_real;
     large_ordered_semiring_1_floor_ceiling = large_ordered_semiring_1_real;
     floor = floor_real}
    : real floor_ceiling);;

let rec equal_iarraya _A asa bs = equal_lista _A (list_of asa) (list_of bs);;

let rec equal_iarray _A = ({equal = equal_iarraya _A} : 'a iarray equal);;

let rec gcdc (_A1, _A2)
  a b = (if eq _A2 b
              (zero _A1.euclidean_semiring_normalization_euclidean_semiring.semidom_modulo_euclidean_semiring.algebraic_semidom_semidom_modulo.semidom_divide_algebraic_semidom.semidom_semidom_divide.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
          then normalizea
                 _A1.factorial_semiring_normalization_euclidean_semiring.normalization_semidom_factorial_semiring
                 a
          else gcdc (_A1, _A2) b
                 (modulo
                   _A1.euclidean_semiring_normalization_euclidean_semiring.semidom_modulo_euclidean_semiring.semiring_modulo_semidom_modulo.modulo_semiring_modulo
                   a b));;

let rec lcmc (_A1, _A2)
  a b = normalizea
          _A1.factorial_semiring_normalization_euclidean_semiring.normalization_semidom_factorial_semiring
          (divide
            _A1.euclidean_semiring_normalization_euclidean_semiring.semidom_modulo_euclidean_semiring.semiring_modulo_semidom_modulo.modulo_semiring_modulo.divide_modulo
            (times
              _A1.euclidean_semiring_normalization_euclidean_semiring.semidom_modulo_euclidean_semiring.semiring_modulo_semidom_modulo.modulo_semiring_modulo.dvd_modulo.times_dvd
              a b)
            (gcdc (_A1, _A2) a b));;

type complex = Complex of real * real;;

let rec euclidean_size_field (_A1, _A2)
  x = (if eq _A2 x
            (zero _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
        then zero_nata else one_nata);;

let rec re (Complex (x1, x2)) = x1;;

let rec im (Complex (x1, x2)) = x2;;

let rec inverse_complexa
  x = Complex
        (divide_reala (re x)
           (plus_reala
             (binary_power monoid_mult_real (re x)
               (nat_of_integer (Z.of_int 2)))
             (binary_power monoid_mult_real (im x)
               (nat_of_integer (Z.of_int 2)))),
          divide_reala (uminus_reala (im x))
            (plus_reala
              (binary_power monoid_mult_real (re x)
                (nat_of_integer (Z.of_int 2)))
              (binary_power monoid_mult_real (im x)
                (nat_of_integer (Z.of_int 2)))));;

let rec uminus_complexa x = Complex (uminus_reala (re x), uminus_reala (im x));;

let rec times_complexa
  x y = Complex
          (minus_reala (times_reala (re x) (re y)) (times_reala (im x) (im y)),
            plus_reala (times_reala (re x) (im y))
              (times_reala (im x) (re y)));;

let rec divide_complexa x y = times_complexa x (inverse_complexa y);;

let rec minus_complexa
  x y = Complex (minus_reala (re x) (re y), minus_reala (im x) (im y));;

let zero_complexa : complex = Complex (zero_reala, zero_reala);;

let rec plus_complexa
  x y = Complex (plus_reala (re x) (re y), plus_reala (im x) (im y));;

let one_complexa : complex = Complex (one_reala, zero_reala);;

let plus_complex = ({plus = plus_complexa} : complex plus);;

let semigroup_add_complex =
  ({plus_semigroup_add = plus_complex} : complex semigroup_add);;

let cancel_semigroup_add_complex =
  ({semigroup_add_cancel_semigroup_add = semigroup_add_complex} :
    complex cancel_semigroup_add);;

let ab_semigroup_add_complex =
  ({semigroup_add_ab_semigroup_add = semigroup_add_complex} :
    complex ab_semigroup_add);;

let minus_complex = ({minus = minus_complexa} : complex minus);;

let cancel_ab_semigroup_add_complex =
  ({ab_semigroup_add_cancel_ab_semigroup_add = ab_semigroup_add_complex;
     cancel_semigroup_add_cancel_ab_semigroup_add =
       cancel_semigroup_add_complex;
     minus_cancel_ab_semigroup_add = minus_complex}
    : complex cancel_ab_semigroup_add);;

let zero_complex = ({zero = zero_complexa} : complex zero);;

let monoid_add_complex =
  ({semigroup_add_monoid_add = semigroup_add_complex;
     zero_monoid_add = zero_complex}
    : complex monoid_add);;

let comm_monoid_add_complex =
  ({ab_semigroup_add_comm_monoid_add = ab_semigroup_add_complex;
     monoid_add_comm_monoid_add = monoid_add_complex}
    : complex comm_monoid_add);;

let cancel_comm_monoid_add_complex =
  ({cancel_ab_semigroup_add_cancel_comm_monoid_add =
      cancel_ab_semigroup_add_complex;
     comm_monoid_add_cancel_comm_monoid_add = comm_monoid_add_complex}
    : complex cancel_comm_monoid_add);;

let times_complex = ({times = times_complexa} : complex times);;

let mult_zero_complex =
  ({times_mult_zero = times_complex; zero_mult_zero = zero_complex} :
    complex mult_zero);;

let semigroup_mult_complex =
  ({times_semigroup_mult = times_complex} : complex semigroup_mult);;

let semiring_complex =
  ({ab_semigroup_add_semiring = ab_semigroup_add_complex;
     semigroup_mult_semiring = semigroup_mult_complex}
    : complex semiring);;

let semiring_0_complex =
  ({comm_monoid_add_semiring_0 = comm_monoid_add_complex;
     mult_zero_semiring_0 = mult_zero_complex;
     semiring_semiring_0 = semiring_complex}
    : complex semiring_0);;

let semiring_0_cancel_complex =
  ({cancel_comm_monoid_add_semiring_0_cancel = cancel_comm_monoid_add_complex;
     semiring_0_semiring_0_cancel = semiring_0_complex}
    : complex semiring_0_cancel);;

let ab_semigroup_mult_complex =
  ({semigroup_mult_ab_semigroup_mult = semigroup_mult_complex} :
    complex ab_semigroup_mult);;

let comm_semiring_complex =
  ({ab_semigroup_mult_comm_semiring = ab_semigroup_mult_complex;
     semiring_comm_semiring = semiring_complex}
    : complex comm_semiring);;

let comm_semiring_0_complex =
  ({comm_semiring_comm_semiring_0 = comm_semiring_complex;
     semiring_0_comm_semiring_0 = semiring_0_complex}
    : complex comm_semiring_0);;

let comm_semiring_0_cancel_complex =
  ({comm_semiring_0_comm_semiring_0_cancel = comm_semiring_0_complex;
     semiring_0_cancel_comm_semiring_0_cancel = semiring_0_cancel_complex}
    : complex comm_semiring_0_cancel);;

let one_complex = ({one = one_complexa} : complex one);;

let power_complex =
  ({one_power = one_complex; times_power = times_complex} : complex power);;

let monoid_mult_complex =
  ({semigroup_mult_monoid_mult = semigroup_mult_complex;
     power_monoid_mult = power_complex}
    : complex monoid_mult);;

let numeral_complex =
  ({one_numeral = one_complex; semigroup_add_numeral = semigroup_add_complex} :
    complex numeral);;

let semiring_numeral_complex =
  ({monoid_mult_semiring_numeral = monoid_mult_complex;
     numeral_semiring_numeral = numeral_complex;
     semiring_semiring_numeral = semiring_complex}
    : complex semiring_numeral);;

let zero_neq_one_complex =
  ({one_zero_neq_one = one_complex; zero_zero_neq_one = zero_complex} :
    complex zero_neq_one);;

let semiring_1_complex =
  ({semiring_numeral_semiring_1 = semiring_numeral_complex;
     semiring_0_semiring_1 = semiring_0_complex;
     zero_neq_one_semiring_1 = zero_neq_one_complex}
    : complex semiring_1);;

let semiring_1_cancel_complex =
  ({semiring_0_cancel_semiring_1_cancel = semiring_0_cancel_complex;
     semiring_1_semiring_1_cancel = semiring_1_complex}
    : complex semiring_1_cancel);;

let dvd_complex = ({times_dvd = times_complex} : complex dvd);;

let comm_monoid_mult_complex =
  ({ab_semigroup_mult_comm_monoid_mult = ab_semigroup_mult_complex;
     monoid_mult_comm_monoid_mult = monoid_mult_complex;
     dvd_comm_monoid_mult = dvd_complex}
    : complex comm_monoid_mult);;

let comm_semiring_1_complex =
  ({comm_monoid_mult_comm_semiring_1 = comm_monoid_mult_complex;
     comm_semiring_0_comm_semiring_1 = comm_semiring_0_complex;
     semiring_1_comm_semiring_1 = semiring_1_complex}
    : complex comm_semiring_1);;

let comm_semiring_1_cancel_complex =
  ({comm_semiring_0_cancel_comm_semiring_1_cancel =
      comm_semiring_0_cancel_complex;
     comm_semiring_1_comm_semiring_1_cancel = comm_semiring_1_complex;
     semiring_1_cancel_comm_semiring_1_cancel = semiring_1_cancel_complex}
    : complex comm_semiring_1_cancel);;

let comm_semiring_1_cancel_crossproduct_complex =
  ({comm_semiring_1_cancel_comm_semiring_1_cancel_crossproduct =
      comm_semiring_1_cancel_complex}
    : complex comm_semiring_1_cancel_crossproduct);;

let semiring_no_zero_divisors_complex =
  ({semiring_0_semiring_no_zero_divisors = semiring_0_complex} :
    complex semiring_no_zero_divisors);;

let semiring_1_no_zero_divisors_complex =
  ({semiring_1_semiring_1_no_zero_divisors = semiring_1_complex;
     semiring_no_zero_divisors_semiring_1_no_zero_divisors =
       semiring_no_zero_divisors_complex}
    : complex semiring_1_no_zero_divisors);;

let semiring_no_zero_divisors_cancel_complex =
  ({semiring_no_zero_divisors_semiring_no_zero_divisors_cancel =
      semiring_no_zero_divisors_complex}
    : complex semiring_no_zero_divisors_cancel);;

let uminus_complex = ({uminus = uminus_complexa} : complex uminus);;

let group_add_complex =
  ({cancel_semigroup_add_group_add = cancel_semigroup_add_complex;
     minus_group_add = minus_complex; monoid_add_group_add = monoid_add_complex;
     uminus_group_add = uminus_complex}
    : complex group_add);;

let ab_group_add_complex =
  ({cancel_comm_monoid_add_ab_group_add = cancel_comm_monoid_add_complex;
     group_add_ab_group_add = group_add_complex}
    : complex ab_group_add);;

let ring_complex =
  ({ab_group_add_ring = ab_group_add_complex;
     semiring_0_cancel_ring = semiring_0_cancel_complex}
    : complex ring);;

let ring_no_zero_divisors_complex =
  ({ring_ring_no_zero_divisors = ring_complex;
     semiring_no_zero_divisors_cancel_ring_no_zero_divisors =
       semiring_no_zero_divisors_cancel_complex}
    : complex ring_no_zero_divisors);;

let neg_numeral_complex =
  ({group_add_neg_numeral = group_add_complex;
     numeral_neg_numeral = numeral_complex}
    : complex neg_numeral);;

let ring_1_complex =
  ({neg_numeral_ring_1 = neg_numeral_complex; ring_ring_1 = ring_complex;
     semiring_1_cancel_ring_1 = semiring_1_cancel_complex}
    : complex ring_1);;

let ring_1_no_zero_divisors_complex =
  ({ring_1_ring_1_no_zero_divisors = ring_1_complex;
     ring_no_zero_divisors_ring_1_no_zero_divisors =
       ring_no_zero_divisors_complex;
     semiring_1_no_zero_divisors_ring_1_no_zero_divisors =
       semiring_1_no_zero_divisors_complex}
    : complex ring_1_no_zero_divisors);;

let comm_ring_complex =
  ({comm_semiring_0_cancel_comm_ring = comm_semiring_0_cancel_complex;
     ring_comm_ring = ring_complex}
    : complex comm_ring);;

let comm_ring_1_complex =
  ({comm_ring_comm_ring_1 = comm_ring_complex;
     comm_semiring_1_cancel_comm_ring_1 = comm_semiring_1_cancel_complex;
     ring_1_comm_ring_1 = ring_1_complex}
    : complex comm_ring_1);;

let semidom_complex =
  ({comm_semiring_1_cancel_semidom = comm_semiring_1_cancel_complex;
     semiring_1_no_zero_divisors_semidom = semiring_1_no_zero_divisors_complex}
    : complex semidom);;

let idom_complex =
  ({comm_ring_1_idom = comm_ring_1_complex;
     ring_1_no_zero_divisors_idom = ring_1_no_zero_divisors_complex;
     semidom_idom = semidom_complex;
     comm_semiring_1_cancel_crossproduct_idom =
       comm_semiring_1_cancel_crossproduct_complex}
    : complex idom);;

let ufd_complex = ({idom_ufd = idom_complex} : complex ufd);;

let divide_complex = ({divide = divide_complexa} : complex divide);;

let inverse_complex =
  ({divide_inverse = divide_complex; inverse = inverse_complexa} :
    complex inverse);;

let division_ring_complex =
  ({inverse_division_ring = inverse_complex;
     ring_1_no_zero_divisors_division_ring = ring_1_no_zero_divisors_complex}
    : complex division_ring);;

let semidom_divide_complex =
  ({divide_semidom_divide = divide_complex;
     semidom_semidom_divide = semidom_complex;
     semiring_no_zero_divisors_cancel_semidom_divide =
       semiring_no_zero_divisors_cancel_complex}
    : complex semidom_divide);;

let idom_divide_complex =
  ({idom_idom_divide = idom_complex;
     semidom_divide_idom_divide = semidom_divide_complex}
    : complex idom_divide);;

let field_complex =
  ({division_ring_field = division_ring_complex;
     idom_divide_field = idom_divide_complex; ufd_field = ufd_complex}
    : complex field);;

let rec equal_complexa
  (Complex (x1, x2)) (Complex (y1, y2)) =
    equal_reala x1 y1 && equal_reala x2 y2;;

let equal_complex = ({equal = equal_complexa} : complex equal);;

let rec euclidean_size_complex
  x = euclidean_size_field (field_complex, equal_complex) x;;

let rec normalize_field (_A1, _A2)
  x = (if eq _A2 x
            (zero _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
        then zero _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
        else one _A1.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.numeral_neg_numeral.one_numeral);;

let rec normalize_complex x = normalize_field (field_complex, equal_complex) x;;

let rec unit_factor_field _A x = x;;

let rec unit_factor_complexa x = unit_factor_field field_complex x;;

let rec mod_field (_A1, _A2)
  x y = (if eq _A2 y
              (zero _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
          then x
          else zero _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero);;

let rec modulo_complexa x = mod_field (field_complex, equal_complex) x;;

let algebraic_semidom_complex =
  ({semidom_divide_algebraic_semidom = semidom_divide_complex} :
    complex algebraic_semidom);;

let modulo_complex =
  ({divide_modulo = divide_complex; dvd_modulo = dvd_complex;
     modulo = modulo_complexa}
    : complex modulo);;

let semiring_modulo_complex =
  ({comm_semiring_1_cancel_semiring_modulo = comm_semiring_1_cancel_complex;
     modulo_semiring_modulo = modulo_complex}
    : complex semiring_modulo);;

let semidom_modulo_complex =
  ({algebraic_semidom_semidom_modulo = algebraic_semidom_complex;
     semiring_modulo_semidom_modulo = semiring_modulo_complex}
    : complex semidom_modulo);;

let euclidean_semiring_complex =
  ({semidom_modulo_euclidean_semiring = semidom_modulo_complex;
     euclidean_size = euclidean_size_complex}
    : complex euclidean_semiring);;

let unit_factor_complex =
  ({unit_factor = unit_factor_complexa} : complex unit_factor);;

let semidom_divide_unit_factor_complex =
  ({semidom_divide_semidom_divide_unit_factor = semidom_divide_complex;
     unit_factor_semidom_divide_unit_factor = unit_factor_complex}
    : complex semidom_divide_unit_factor);;

let normalization_semidom_complex =
  ({algebraic_semidom_normalization_semidom = algebraic_semidom_complex;
     semidom_divide_unit_factor_normalization_semidom =
       semidom_divide_unit_factor_complex;
     normalizea = normalize_complex}
    : complex normalization_semidom);;

let factorial_semiring_complex =
  ({normalization_semidom_factorial_semiring = normalization_semidom_complex} :
    complex factorial_semiring);;

let normalization_euclidean_semiring_complex =
  ({euclidean_semiring_normalization_euclidean_semiring =
      euclidean_semiring_complex;
     factorial_semiring_normalization_euclidean_semiring =
       factorial_semiring_complex}
    : complex normalization_euclidean_semiring);;

let rec lcm_complexa
  x = lcmc (normalization_euclidean_semiring_complex, equal_complex) x;;

let rec gcd_complexc
  x = gcdc (normalization_euclidean_semiring_complex, equal_complex) x;;

let lcmb _ = failwith
  "Euclidean_Algorithm.normalization_euclidean_semiring_class.Lcm";;

let rec lcm_complex x = lcmb x;;

let gcdb _ = failwith
  "Euclidean_Algorithm.normalization_euclidean_semiring_class.Gcd";;

let rec gcd_complexb x = gcdb x;;

let gcd_complexa =
  ({one_gcd = one_complex; zero_gcd = zero_complex; dvd_gcd = dvd_complex;
     gcda = gcd_complexc; lcma = lcm_complexa}
    : complex gcda);;

let gcd_complex =
  ({gcd_Gcd = gcd_complexa; gcd = gcd_complexb; lcm = lcm_complex} :
    complex gcd);;

let comm_monoid_gcd_complex =
  ({gcd_comm_monoid_gcd = gcd_complexa;
     comm_semiring_1_comm_monoid_gcd = comm_semiring_1_complex}
    : complex comm_monoid_gcd);;

let idom_gcd_complex =
  ({idom_idom_gcd = idom_complex;
     comm_monoid_gcd_idom_gcd = comm_monoid_gcd_complex}
    : complex idom_gcd);;

let semiring_gcd_complex =
  ({normalization_semidom_semiring_gcd = normalization_semidom_complex;
     comm_monoid_gcd_semiring_gcd = comm_monoid_gcd_complex}
    : complex semiring_gcd);;

let ring_gcd_complex =
  ({semiring_gcd_ring_gcd = semiring_gcd_complex;
     idom_gcd_ring_gcd = idom_gcd_complex}
    : complex ring_gcd);;

let semiring_char_0_complex =
  ({semiring_1_semiring_char_0 = semiring_1_complex} :
    complex semiring_char_0);;

let ring_char_0_complex =
  ({semiring_char_0_ring_char_0 = semiring_char_0_complex;
     ring_1_ring_char_0 = ring_1_complex}
    : complex ring_char_0);;

let semiring_Gcd_complex =
  ({gcd_semiring_Gcd = gcd_complex;
     semiring_gcd_semiring_Gcd = semiring_gcd_complex}
    : complex semiring_Gcd);;

let idom_modulo_complex =
  ({idom_divide_idom_modulo = idom_divide_complex;
     semidom_modulo_idom_modulo = semidom_modulo_complex}
    : complex idom_modulo);;

let rec cnj z = Complex (re z, uminus_reala (im z));;

let rec conjugate_complexa x = cnj x;;

type 'a conjugate = {conjugate : 'a -> 'a};;
let conjugate _A = _A.conjugate;;

let conjugate_complex = ({conjugate = conjugate_complexa} : complex conjugate);;

let field_char_0_complex =
  ({field_field_char_0 = field_complex;
     ring_char_0_field_char_0 = ring_char_0_complex}
    : complex field_char_0);;

let rec to_rat_2 = function Rational r -> Some r
                   | Irrational (n, rai) -> None;;

let rec to_rat_3 xa = to_rat_2 (rep_real_alg_3 xa);;

let rec to_rat_real_alg_main (Real_Alg_Quotient xa) = to_rat_3 xa;;

let rec to_rat_real_alg
  x = (match to_rat_real_alg_main x with None -> zero_rata | Some q -> q);;

let rec to_rat_real (Real_of x) = to_rat_real_alg x;;

let rec is_rat_real_alg
  x = (match to_rat_real_alg_main x with None -> false | Some _ -> true);;

let rec is_rat_real (Real_of x) = is_rat_real_alg x;;

let rec to_rat_complex
  x = (if is_rat_real (re x) && equal_reala (im x) zero_reala
        then to_rat_real (re x) else zero_rata);;

let rec is_rat_complexa
  x = is_rat_real (re x) && equal_reala (im x) zero_reala;;

type 'a is_rat =
  {field_char_0_is_rat : 'a field_char_0; is_rat : 'a -> bool;
    to_rat : 'a -> rat};;
let is_rat _A = _A.is_rat;;
let to_rat _A = _A.to_rat;;

let is_rat_complex =
  ({field_char_0_is_rat = field_char_0_complex; is_rat = is_rat_complexa;
     to_rat = to_rat_complex}
    : complex is_rat);;

type 'a conjugatable_ring =
  {conjugate_conjugatable_ring : 'a conjugate;
    ring_conjugatable_ring : 'a ring};;

let conjugatable_ring_complex =
  ({conjugate_conjugatable_ring = conjugate_complex;
     ring_conjugatable_ring = ring_complex}
    : complex conjugatable_ring);;

type 'a conjugatable_field =
  {conjugatable_ring_conjugatable_field : 'a conjugatable_ring;
    field_conjugatable_field : 'a field};;

let conjugatable_field_complex =
  ({conjugatable_ring_conjugatable_field = conjugatable_ring_complex;
     field_conjugatable_field = field_complex}
    : complex conjugatable_field);;

let normalization_semidom_multiplicative_complex =
  ({normalization_semidom_normalization_semidom_multiplicative =
      normalization_semidom_complex}
    : complex normalization_semidom_multiplicative);;

let semiring_gcd_mult_normalize_complex =
  ({semiring_gcd_semiring_gcd_mult_normalize = semiring_gcd_complex;
     normalization_semidom_multiplicative_semiring_gcd_mult_normalize =
       normalization_semidom_multiplicative_complex}
    : complex semiring_gcd_mult_normalize);;

let euclidean_ring_complex =
  ({euclidean_semiring_euclidean_ring = euclidean_semiring_complex;
     idom_modulo_euclidean_ring = idom_modulo_complex}
    : complex euclidean_ring);;

let factorial_semiring_gcd_complex =
  ({factorial_semiring_factorial_semiring_gcd = factorial_semiring_complex;
     semiring_Gcd_factorial_semiring_gcd = semiring_Gcd_complex}
    : complex factorial_semiring_gcd);;

let factorial_ring_gcd_complex =
  ({factorial_semiring_gcd_factorial_ring_gcd = factorial_semiring_gcd_complex;
     ring_gcd_factorial_ring_gcd = ring_gcd_complex;
     idom_divide_factorial_ring_gcd = idom_divide_complex}
    : complex factorial_ring_gcd);;

let euclidean_semiring_gcd_complex =
  ({normalization_euclidean_semiring_euclidean_semiring_gcd =
      normalization_euclidean_semiring_complex;
     factorial_semiring_gcd_euclidean_semiring_gcd =
       factorial_semiring_gcd_complex}
    : complex euclidean_semiring_gcd);;

let euclidean_ring_gcd_complex =
  ({euclidean_semiring_gcd_euclidean_ring_gcd = euclidean_semiring_gcd_complex;
     euclidean_ring_euclidean_ring_gcd = euclidean_ring_complex;
     factorial_ring_gcd_euclidean_ring_gcd = factorial_ring_gcd_complex}
    : complex euclidean_ring_gcd);;

type ('a, 'b) fmap = Fmap_of_list of ('a * 'b) list;;

let rec fmlookup _A (Fmap_of_list m) = map_of _A m;;

let rec fmlookup_default _B
  d m x = (match fmlookup _B m x with None -> d | Some v -> v);;

type ('a, 'b) poly_mapping = Pm_fmap of ('a, 'b) fmap;;

type 'a fset = Abs_fset of 'a set;;

let rec fun_upd equal f aa b a = (if equal aa a then b else f a);;

let rec balance_right
  a k x xa3 = match a, k, x, xa3 with
    a, k, x, Branch (R, b, s, y, c) ->
      Branch (R, a, k, x, Branch (B, b, s, y, c))
    | Branch (B, a, k, x, b), s, y, Empty ->
        balance (Branch (R, a, k, x, b)) s y Empty
    | Branch (B, a, k, x, b), s, y, Branch (B, va, vb, vc, vd) ->
        balance (Branch (R, a, k, x, b)) s y (Branch (B, va, vb, vc, vd))
    | Branch (R, a, k, x, Branch (B, b, s, y, c)), t, z, Empty ->
        Branch (R, balance (paint R a) k x b, s, y, Branch (B, c, t, z, Empty))
    | Branch (R, a, k, x, Branch (B, b, s, y, c)), t, z,
        Branch (B, va, vb, vc, vd)
        -> Branch
             (R, balance (paint R a) k x b, s, y,
               Branch (B, c, t, z, Branch (B, va, vb, vc, vd)))
    | Empty, k, x, Empty -> Empty
    | Branch (R, va, vb, vc, Empty), k, x, Empty -> Empty
    | Branch (R, va, vb, vc, Branch (R, ve, vf, vg, vh)), k, x, Empty -> Empty
    | Empty, k, x, Branch (B, va, vb, vc, vd) -> Empty
    | Branch (R, ve, vf, vg, Empty), k, x, Branch (B, va, vb, vc, vd) -> Empty
    | Branch (R, ve, vf, vg, Branch (R, vi, vj, vk, vl)), k, x,
        Branch (B, va, vb, vc, vd)
        -> Empty;;

let rec balance_left
  x0 s y c = match x0, s, y, c with
    Branch (R, a, k, x, b), s, y, c ->
      Branch (R, Branch (B, a, k, x, b), s, y, c)
    | Empty, k, x, Branch (B, a, s, y, b) ->
        balance Empty k x (Branch (R, a, s, y, b))
    | Branch (B, va, vb, vc, vd), k, x, Branch (B, a, s, y, b) ->
        balance (Branch (B, va, vb, vc, vd)) k x (Branch (R, a, s, y, b))
    | Empty, k, x, Branch (R, Branch (B, a, s, y, b), t, z, c) ->
        Branch (R, Branch (B, Empty, k, x, a), s, y, balance b t z (paint R c))
    | Branch (B, va, vb, vc, vd), k, x,
        Branch (R, Branch (B, a, s, y, b), t, z, c)
        -> Branch
             (R, Branch (B, Branch (B, va, vb, vc, vd), k, x, a), s, y,
               balance b t z (paint R c))
    | Empty, k, x, Empty -> Empty
    | Empty, k, x, Branch (R, Empty, vb, vc, vd) -> Empty
    | Empty, k, x, Branch (R, Branch (R, ve, vf, vg, vh), vb, vc, vd) -> Empty
    | Branch (B, va, vb, vc, vd), k, x, Empty -> Empty
    | Branch (B, va, vb, vc, vd), k, x, Branch (R, Empty, vf, vg, vh) -> Empty
    | Branch (B, va, vb, vc, vd), k, x,
        Branch (R, Branch (R, vi, vj, vk, vl), vf, vg, vh)
        -> Empty;;

let rec combine
  xa0 x = match xa0, x with Empty, x -> x
    | Branch (v, va, vb, vc, vd), Empty -> Branch (v, va, vb, vc, vd)
    | Branch (R, a, k, x, b), Branch (R, c, s, y, d) ->
        (match combine b c
          with Empty -> Branch (R, a, k, x, Branch (R, Empty, s, y, d))
          | Branch (R, b2, t, z, c2) ->
            Branch (R, Branch (R, a, k, x, b2), t, z, Branch (R, c2, s, y, d))
          | Branch (B, b2, t, z, c2) ->
            Branch (R, a, k, x, Branch (R, Branch (B, b2, t, z, c2), s, y, d)))
    | Branch (B, a, k, x, b), Branch (B, c, s, y, d) ->
        (match combine b c
          with Empty -> balance_left a k x (Branch (B, Empty, s, y, d))
          | Branch (R, b2, t, z, c2) ->
            Branch (R, Branch (B, a, k, x, b2), t, z, Branch (B, c2, s, y, d))
          | Branch (B, b2, t, z, c2) ->
            balance_left a k x (Branch (B, Branch (B, b2, t, z, c2), s, y, d)))
    | Branch (B, va, vb, vc, vd), Branch (R, b, k, x, c) ->
        Branch (R, combine (Branch (B, va, vb, vc, vd)) b, k, x, c)
    | Branch (R, a, k, x, b), Branch (B, va, vb, vc, vd) ->
        Branch (R, a, k, x, combine b (Branch (B, va, vb, vc, vd)));;

let rec rbt_comp_del
  c x xa2 = match c, x, xa2 with c, x, Empty -> Empty
    | c, x, Branch (uu, a, y, s, b) ->
        (match c x y with Eq -> combine a b
          | Lt -> rbt_comp_del_from_left c x a y s b
          | Gt -> rbt_comp_del_from_right c x a y s b)
and rbt_comp_del_from_left
  c x xa2 y s b = match c, x, xa2, y, s, b with
    c, x, Branch (B, lt, z, v, rt), y, s, b ->
      balance_left (rbt_comp_del c x (Branch (B, lt, z, v, rt))) y s b
    | c, x, Empty, y, s, b -> Branch (R, rbt_comp_del c x Empty, y, s, b)
    | c, x, Branch (R, va, vb, vc, vd), y, s, b ->
        Branch (R, rbt_comp_del c x (Branch (R, va, vb, vc, vd)), y, s, b)
and rbt_comp_del_from_right
  c x a y s xa5 = match c, x, a, y, s, xa5 with
    c, x, a, y, s, Branch (B, lt, z, v, rt) ->
      balance_right a y s (rbt_comp_del c x (Branch (B, lt, z, v, rt)))
    | c, x, a, y, s, Empty -> Branch (R, a, y, s, rbt_comp_del c x Empty)
    | c, x, a, y, s, Branch (R, va, vb, vc, vd) ->
        Branch (R, a, y, s, rbt_comp_del c x (Branch (R, va, vb, vc, vd)));;

let rec rbt_comp_delete c k t = paint B (rbt_comp_del c k t);;

let rec delete _A
  xb xc = Mapping_RBT (rbt_comp_delete (the (ccompare _A)) xb (impl_of _A xc));;

let rec list_remove1
  equal x xa2 = match equal, x, xa2 with
    equal, x, y :: xs ->
      (if equal x y then xs else y :: list_remove1 equal x xs)
    | equal, x, [] -> [];;

let rec removea _A
  xb xc = Abs_dlist (list_remove1 (the (ceq _A)) xb (list_of_dlist _A xc));;

let rec insert (_A1, _A2)
  xa x1 = match xa, x1 with
    xa, Complement x -> Complement (remove (_A1, _A2) xa x)
    | x, RBT_set rbt ->
        (match ccompare _A2
          with None ->
            failwith "insert RBT_set: ccompare = None"
              (fun _ -> insert (_A1, _A2) x (RBT_set rbt))
          | Some _ -> RBT_set (insertb _A2 x () rbt))
    | x, DList_set dxs ->
        (match ceq _A1
          with None ->
            failwith "insert DList_set: ceq = None"
              (fun _ -> insert (_A1, _A2) x (DList_set dxs))
          | Some _ -> DList_set (inserta _A1 x dxs))
    | x, Set_Monad xs -> Set_Monad (x :: xs)
    | x, Collect_set a ->
        (match ceq _A1
          with None ->
            failwith "insert Collect_set: ceq = None"
              (fun _ -> insert (_A1, _A2) x (Collect_set a))
          | Some eq -> Collect_set (fun_upd eq a x true))
and remove (_A1, _A2)
  x xa1 = match x, xa1 with
    x, Complement a -> Complement (insert (_A1, _A2) x a)
    | x, RBT_set rbt ->
        (match ccompare _A2
          with None ->
            failwith "remove RBT_set: ccompare = None"
              (fun _ -> remove (_A1, _A2) x (RBT_set rbt))
          | Some _ -> RBT_set (delete _A2 x rbt))
    | x, DList_set dxs ->
        (match ceq _A1
          with None ->
            failwith "remove DList_set: ceq = None"
              (fun _ -> remove (_A1, _A2) x (DList_set dxs))
          | Some _ -> DList_set (removea _A1 x dxs))
    | x, Collect_set a ->
        (match ceq _A1
          with None ->
            failwith "remove Collect: ceq = None"
              (fun _ -> remove (_A1, _A2) x (Collect_set a))
          | Some eq -> Collect_set (fun_upd eq a x false));;

let rec foldl f a x2 = match f, a, x2 with f, a, [] -> a
                | f, a, x :: xs -> foldl f (f a x) xs;;

let rec set_aux (_A1, _A2)
  = function Set_Monada -> (fun a -> Set_Monad a)
    | Set_Choose ->
        (match ccompare _A2
          with None ->
            (match ceq _A1 with None -> (fun a -> Set_Monad a)
              | Some _ ->
                foldl (fun s x -> insert (_A1, _A2) x s)
                  (DList_set (empty _A1)))
          | Some _ ->
            foldl (fun s x -> insert (_A1, _A2) x s) (RBT_set (emptya _A2)))
    | impl ->
        foldl (fun s x -> insert (_A1, _A2) x s) (set_empty (_A1, _A2) impl);;

let rec set (_A1, _A2, _A3)
  xs = set_aux (_A1, _A2) (of_phantom (set_impl _A3)) xs;;

let rec fset_of_list (_A1, _A2, _A3) xa = Abs_fset (set (_A1, _A2, _A3) xa);;

let rec fset (Abs_fset x) = x;;

let rec foldb _A x xc = folda (fun a _ -> x a) (impl_of _A xc);;

let rec image (_A1, _A2) (_B1, _B2, _B3)
  h x1 = match h, x1 with
    h, RBT_set rbt ->
      (match ccompare _A2
        with None ->
          failwith "image RBT_set: ccompare = None"
            (fun _ -> image (_A1, _A2) (_B1, _B2, _B3) h (RBT_set rbt))
        | Some _ ->
          foldb _A2 (comp (insert (_B1, _B2)) h) rbt (bot_set (_B1, _B2, _B3)))
    | g, DList_set dxs ->
        (match ceq _A1
          with None ->
            failwith "image DList_set: ceq = None"
              (fun _ -> image (_A1, _A2) (_B1, _B2, _B3) g (DList_set dxs))
          | Some _ ->
            foldc _A1 (comp (insert (_B1, _B2)) g) dxs
              (bot_set (_B1, _B2, _B3)))
    | f, Complement (Complement b) -> image (_A1, _A2) (_B1, _B2, _B3) f b
    | f, Collect_set a ->
        failwith "image Collect_set"
          (fun _ -> image (_A1, _A2) (_B1, _B2, _B3) f (Collect_set a))
    | f, Set_Monad xs -> Set_Monad (map f xs);;

let rec fimage (_B1, _B2) (_A1, _A2, _A3)
  xb xc = Abs_fset (image (_B1, _B2) (_A1, _A2, _A3) xb (fset xc));;

let rec comparator_prod
  comp_a comp_b (x, xa) (y, ya) =
    (match comp_a x y with Eq -> comp_b xa ya | Lt -> Lt | Gt -> Gt);;

let rec ccompare_proda _A _B
  = (match ccompare _A with None -> None
      | Some comp_a ->
        (match ccompare _B with None -> None
          | Some comp_b -> Some (comparator_prod comp_a comp_b)));;

let rec ccompare_prod _A _B =
  ({ccompare = ccompare_proda _A _B} : ('a * 'b) ccompare);;

let rec set_impl_choose2
  x y = match x, y with Set_Monada, Set_Monada -> Set_Monada
    | Set_RBT, Set_RBT -> Set_RBT
    | Set_DList, Set_DList -> Set_DList
    | Set_Collect, Set_Collect -> Set_Collect
    | x, y -> Set_Choose;;

let rec set_impl_proda _A _B
  = Phantom
      (set_impl_choose2 (of_phantom (set_impl _A)) (of_phantom (set_impl _B)));;

let rec set_impl_prod _A _B =
  ({set_impl = set_impl_proda _A _B} : ('a * 'b) set_impl);;

let rec equality_prod eq_a eq_b (x, xa) (y, ya) = eq_a x y && eq_b xa ya;;

let rec ceq_proda _A _B
  = (match ceq _A with None -> None
      | Some eq_a ->
        (match ceq _B with None -> None
          | Some eq_b -> Some (equality_prod eq_a eq_b)));;

let rec ceq_prod _A _B = ({ceq = ceq_proda _A _B} : ('a * 'b) ceq);;

let rec fmdom (_A1, _A2, _A3) (_B1, _B2, _B3)
  (Fmap_of_list m) =
    fimage ((ceq_prod _A1 _B1), (ccompare_prod _A2 _B2)) (_A1, _A2, _A3) fst
      (fset_of_list
        ((ceq_prod _A1 _B1), (ccompare_prod _A2 _B2), (set_impl_prod _A3 _B3))
        m);;

let rec rBT_Impl_rbt_all
  p x1 = match p, x1 with
    p, Branch (c, l, k, v, r) ->
      p k v && (rBT_Impl_rbt_all p l && rBT_Impl_rbt_all p r)
    | p, Empty -> true;;

let rec all _A xb xc = rBT_Impl_rbt_all xb (impl_of _A xc);;

let rec ball (_A1, _A2)
  x0 p = match x0, p with
    RBT_set rbt, p ->
      (match ccompare _A2
        with None ->
          failwith "Ball RBT_set: ccompare = None"
            (fun _ -> ball (_A1, _A2) (RBT_set rbt) p)
        | Some _ -> all _A2 (fun k _ -> p k) rbt)
    | DList_set dxs, p ->
        (match ceq _A1
          with None ->
            failwith "Ball DList_set: ceq = None"
              (fun _ -> ball (_A1, _A2) (DList_set dxs) p)
          | Some _ -> dlist_all _A1 p dxs)
    | Set_Monad xs, p -> list_all p xs;;

let rec fBall (_A1, _A2) xa = ball (_A1, _A2) (fset xa);;

let rec fmpred (_A1, _A2, _A3, _A4) (_B1, _B2, _B3)
  p m = fBall (_A1, _A2) (fmdom (_A1, _A2, _A4) (_B1, _B2, _B3) m)
          (fun x -> p x (the (fmlookup _A3 m x)));;

let rec update _A
  k v x2 = match k, v, x2 with k, v, [] -> [(k, v)]
    | k, v, p :: ps ->
        (if eq _A (fst p) k then (k, v) :: ps else p :: update _A k v ps);;

let rec merge _A qs ps = foldr (fun (a, b) -> update _A a b) ps qs;;

let rec fmadd _A
  (Fmap_of_list m) (Fmap_of_list n) = Fmap_of_list (merge _A m n);;

let rec equal_poly_mappinga (_A1, _A2, _A3, _A4) (_B1, _B2, _B3, _B4, _B5)
  (Pm_fmap xs) (Pm_fmap ys) =
    fmpred (_A1, _A2, _A3, _A4) (_B1, _B2, _B5)
      (fun k _ ->
        eq _B4 (fmlookup_default _A3 (zero _B3) xs k)
          (fmlookup_default _A3 (zero _B3) ys k))
      (fmadd _A3 xs ys);;

type 'a mpoly = MPoly of ((nat, nat) poly_mapping, 'a) poly_mapping;;

let ccompare_poly_mappinga :
  (('a, 'b) poly_mapping -> ('a, 'b) poly_mapping -> ordera) option
  = None;;

let ccompare_poly_mapping =
  ({ccompare = ccompare_poly_mappinga} : ('a, 'b) poly_mapping ccompare);;

let set_impl_poly_mappinga : (('a, 'b) poly_mapping, set_impla) phantom
  = Phantom Set_DList;;

let set_impl_poly_mapping =
  ({set_impl = set_impl_poly_mappinga} : ('a, 'b) poly_mapping set_impl);;

let rec equal_poly_mapping (_A1, _A2, _A3, _A4) (_B1, _B2, _B3, _B4, _B5) =
  ({equal = equal_poly_mappinga (_A1, _A2, _A3, _A4) (_B1, _B2, _B3, _B4, _B5)}
    : ('a, 'b) poly_mapping equal);;

let rec ceq_poly_mappinga (_A1, _A2, _A3, _A4) (_B1, _B2, _B3, _B4, _B5)
  = Some (eq (equal_poly_mapping (_A1, _A2, _A3, _A4)
               (_B1, _B2, _B3, _B4, _B5)));;

let rec ceq_poly_mapping (_A1, _A2, _A3, _A4) (_B1, _B2, _B3, _B4, _B5) =
  ({ceq = ceq_poly_mappinga (_A1, _A2, _A3, _A4) (_B1, _B2, _B3, _B4, _B5)} :
    ('a, 'b) poly_mapping ceq);;

let rec equal_mpolya (_A1, _A2, _A3, _A4, _A5)
  (MPoly xc) (MPoly xa) =
    equal_poly_mappinga
      ((ceq_poly_mapping (ceq_nat, ccompare_nat, equal_nat, set_impl_nat)
         (ceq_nat, ccompare_nat, zero_nat, equal_nat, set_impl_nat)),
        ccompare_poly_mapping,
        (equal_poly_mapping (ceq_nat, ccompare_nat, equal_nat, set_impl_nat)
          (ceq_nat, ccompare_nat, zero_nat, equal_nat, set_impl_nat)),
        set_impl_poly_mapping)
      (_A1, _A2, _A3, _A4, _A5) xc xa;;

let rec equal_mpoly (_A1, _A2, _A3, _A4, _A5) =
  ({equal = equal_mpolya (_A1, _A2, _A3, _A4, _A5)} : 'a mpoly equal);;

let rec fmfilter
  p (Fmap_of_list m) = Fmap_of_list (filtera (fun (k, _) -> p k) m);;

let rec clearjunk0 _A (_B1, _B2)
  m = fmfilter
        (fun k -> not (equal_option _B2 (fmlookup _A m k) (Some (zero _B1))))
        m;;

let rec fmmap_keys
  f (Fmap_of_list m) = Fmap_of_list (map (fun (a, b) -> (a, f a b)) m);;

let rec plus_poly_mappinga _A (_B1, _B2)
  (Pm_fmap xs) (Pm_fmap ys) =
    Pm_fmap
      (clearjunk0 _A (_B1.zero_monoid_add, _B2)
        (fmmap_keys
          (fun k _ ->
            plus _B1.semigroup_add_monoid_add.plus_semigroup_add
              (fmlookup_default _A (zero _B1.zero_monoid_add) xs k)
              (fmlookup_default _A (zero _B1.zero_monoid_add) ys k))
          (fmadd _A xs ys)));;

type 'a comm_powerprod =
  {cancel_comm_monoid_add_comm_powerprod : 'a cancel_comm_monoid_add};;

let rec shift_map_keys_punit _A
  t f (Fmap_of_list xs) =
    Fmap_of_list
      (map (fun (k, v) ->
             (plus _A.cancel_comm_monoid_add_comm_powerprod.comm_monoid_add_cancel_comm_monoid_add.monoid_add_comm_monoid_add.semigroup_add_monoid_add.plus_semigroup_add
                t k,
               f v))
        xs);;

let fmempty : ('a, 'b) fmap = Fmap_of_list [];;

let rec monom_mult_punit (_A1, _A2) _B
  c t (Pm_fmap xs) =
    Pm_fmap
      (if eq _A1 c (zero _A2.mult_zero_semiring_0.zero_mult_zero) then fmempty
        else shift_map_keys_punit _B t
               (times _A2.mult_zero_semiring_0.times_mult_zero c) xs);;

let rec except (_A1, _A2) _B
  (Pm_fmap xs) s =
    Pm_fmap (fmfilter (fun k -> not (member (_A1, _A2) k s)) xs);;

let rec times_poly_mapping (_A1, _A2, _A3, _A4, _A5) (_B1, _B2)
  (Pm_fmap (Fmap_of_list xs)) q =
    (match xs with [] -> Pm_fmap fmempty
      | (t, c) :: ys ->
        plus_poly_mappinga _A3
          (_B2.comm_monoid_add_semiring_0.monoid_add_comm_monoid_add, _B1)
          (monom_mult_punit (_B1, _B2) _A4 c t q)
          (times_poly_mapping (_A1, _A2, _A3, _A4, _A5) (_B1, _B2)
            (except (_A1, _A2) _B2.mult_zero_semiring_0.zero_mult_zero
              (Pm_fmap (Fmap_of_list ys))
              (insert (_A1, _A2) t (bot_set (_A1, _A2, _A5))))
            q));;

let rec minus_poly_mappinga _A (_B1, _B2)
  (Pm_fmap xs) (Pm_fmap ys) =
    Pm_fmap
      (clearjunk0 _A
        (_B1.comm_monoid_add_cancel_comm_monoid_add.monoid_add_comm_monoid_add.zero_monoid_add,
          _B2)
        (fmmap_keys
          (fun k _ ->
            minus _B1.cancel_ab_semigroup_add_cancel_comm_monoid_add.minus_cancel_ab_semigroup_add
              (fmlookup_default _A
                (zero _B1.comm_monoid_add_cancel_comm_monoid_add.monoid_add_comm_monoid_add.zero_monoid_add)
                xs k)
              (fmlookup_default _A
                (zero _B1.comm_monoid_add_cancel_comm_monoid_add.monoid_add_comm_monoid_add.zero_monoid_add)
                ys k))
          (fmadd _A xs ys)));;

let rec zero_poly_mappinga _B = Pm_fmap fmempty;;

let rec plus_poly_mapping _A (_B1, _B2) =
  ({plus = plus_poly_mappinga _A (_B1, _B2)} : ('a, 'b) poly_mapping plus);;

let rec semigroup_add_poly_mapping _A (_B1, _B2) =
  ({plus_semigroup_add = (plus_poly_mapping _A (_B1, _B2))} :
    ('a, 'b) poly_mapping semigroup_add);;

let rec cancel_semigroup_add_poly_mapping _A (_B1, _B2) =
  ({semigroup_add_cancel_semigroup_add =
      (semigroup_add_poly_mapping _A
        (_B1.comm_monoid_add_cancel_comm_monoid_add.monoid_add_comm_monoid_add,
          _B2))}
    : ('a, 'b) poly_mapping cancel_semigroup_add);;

let rec ab_semigroup_add_poly_mapping _A (_B1, _B2) =
  ({semigroup_add_ab_semigroup_add =
      (semigroup_add_poly_mapping _A (_B1.monoid_add_comm_monoid_add, _B2))}
    : ('a, 'b) poly_mapping ab_semigroup_add);;

let rec minus_poly_mapping _A (_B1, _B2) =
  ({minus = minus_poly_mappinga _A (_B1, _B2)} : ('a, 'b) poly_mapping minus);;

let rec cancel_ab_semigroup_add_poly_mapping _A (_B1, _B2) =
  ({ab_semigroup_add_cancel_ab_semigroup_add =
      (ab_semigroup_add_poly_mapping _A
        (_B1.comm_monoid_add_cancel_comm_monoid_add, _B2));
     cancel_semigroup_add_cancel_ab_semigroup_add =
       (cancel_semigroup_add_poly_mapping _A (_B1, _B2));
     minus_cancel_ab_semigroup_add = (minus_poly_mapping _A (_B1, _B2))}
    : ('a, 'b) poly_mapping cancel_ab_semigroup_add);;

let rec zero_poly_mapping _B =
  ({zero = zero_poly_mappinga _B} : ('a, 'b) poly_mapping zero);;

let rec monoid_add_poly_mapping _A (_B1, _B2) =
  ({semigroup_add_monoid_add = (semigroup_add_poly_mapping _A (_B1, _B2));
     zero_monoid_add = (zero_poly_mapping _B1.zero_monoid_add)}
    : ('a, 'b) poly_mapping monoid_add);;

let rec comm_monoid_add_poly_mapping _A (_B1, _B2) =
  ({ab_semigroup_add_comm_monoid_add =
      (ab_semigroup_add_poly_mapping _A (_B1, _B2));
     monoid_add_comm_monoid_add =
       (monoid_add_poly_mapping _A (_B1.monoid_add_comm_monoid_add, _B2))}
    : ('a, 'b) poly_mapping comm_monoid_add);;

let rec cancel_comm_monoid_add_poly_mapping _A (_B1, _B2) =
  ({cancel_ab_semigroup_add_cancel_comm_monoid_add =
      (cancel_ab_semigroup_add_poly_mapping _A (_B1, _B2));
     comm_monoid_add_cancel_comm_monoid_add =
       (comm_monoid_add_poly_mapping _A
         (_B1.comm_monoid_add_cancel_comm_monoid_add, _B2))}
    : ('a, 'b) poly_mapping cancel_comm_monoid_add);;

let rec comm_powerprod_poly_mapping _A (_B1, _B2) =
  ({cancel_comm_monoid_add_comm_powerprod =
      (cancel_comm_monoid_add_poly_mapping _A (_B1, _B2))}
    : ('a, 'b) poly_mapping comm_powerprod);;

let rec times_mpolya (_A1, _A2)
  (MPoly xa) (MPoly x) =
    MPoly (times_poly_mapping
            ((ceq_poly_mapping (ceq_nat, ccompare_nat, equal_nat, set_impl_nat)
               (ceq_nat, ccompare_nat,
                 cancel_comm_monoid_add_nat.comm_monoid_add_cancel_comm_monoid_add.monoid_add_comm_monoid_add.zero_monoid_add,
                 equal_nat, set_impl_nat)),
              ccompare_poly_mapping,
              (equal_poly_mapping
                (ceq_nat, ccompare_nat, equal_nat, set_impl_nat)
                (ceq_nat, ccompare_nat,
                  cancel_comm_monoid_add_nat.comm_monoid_add_cancel_comm_monoid_add.monoid_add_comm_monoid_add.zero_monoid_add,
                  equal_nat, set_impl_nat)),
              (comm_powerprod_poly_mapping equal_nat
                (cancel_comm_monoid_add_nat, equal_nat)),
              set_impl_poly_mapping)
            (_A1, _A2) xa x);;

let rec times_mpoly (_A1, _A2) =
  ({times = times_mpolya (_A1, _A2)} : 'a mpoly times);;

let rec dvd_mpoly (_A1, _A2) =
  ({times_dvd =
      (times_mpoly (_A1, _A2.semiring_1_comm_semiring_1.semiring_0_semiring_1))}
    : 'a mpoly dvd);;

let rec sparse_0 _B xs = Pm_fmap (Fmap_of_list xs);;

let rec one_poly_mapping _A _B
  = sparse_0 _B.zero_zero_neq_one [(zero _A, one _B.one_zero_neq_one)];;

let rec one_mpolya _A
  = MPoly (one_poly_mapping (zero_poly_mapping zero_nat) _A);;

let rec one_mpoly _A = ({one = one_mpolya _A} : 'a mpoly one);;

let rec uminus_poly_mapping _A _B
  (Pm_fmap ys) =
    Pm_fmap
      (fmmap_keys
        (fun k _ ->
          uminus _B.group_add_ab_group_add.uminus_group_add
            (fmlookup_default _A
              (zero _B.group_add_ab_group_add.monoid_add_group_add.zero_monoid_add)
              ys k))
        ys);;

let rec uminus_mpolya _A
  (MPoly x) =
    MPoly (uminus_poly_mapping
            (equal_poly_mapping (ceq_nat, ccompare_nat, equal_nat, set_impl_nat)
              (ceq_nat, ccompare_nat, zero_nat, equal_nat, set_impl_nat))
            _A x);;

let rec minus_mpolya (_A1, _A2)
  (MPoly xa) (MPoly x) =
    MPoly (minus_poly_mappinga
            (equal_poly_mapping (ceq_nat, ccompare_nat, equal_nat, set_impl_nat)
              (ceq_nat, ccompare_nat, zero_nat, equal_nat, set_impl_nat))
            (_A1, _A2) xa x);;

let rec zero_mpolya _A = MPoly (zero_poly_mappinga _A);;

let rec plus_mpolya (_A1, _A2)
  (MPoly xa) (MPoly x) =
    MPoly (plus_poly_mappinga
            (equal_poly_mapping (ceq_nat, ccompare_nat, equal_nat, set_impl_nat)
              (ceq_nat, ccompare_nat, zero_nat, equal_nat, set_impl_nat))
            (_A1, _A2) xa x);;

let rec plus_mpoly (_A1, _A2) =
  ({plus = plus_mpolya (_A1, _A2)} : 'a mpoly plus);;

let rec semigroup_add_mpoly (_A1, _A2) =
  ({plus_semigroup_add = (plus_mpoly (_A1, _A2))} : 'a mpoly semigroup_add);;

let rec cancel_semigroup_add_mpoly (_A1, _A2) =
  ({semigroup_add_cancel_semigroup_add =
      (semigroup_add_mpoly
        (_A1.comm_monoid_add_cancel_comm_monoid_add.monoid_add_comm_monoid_add,
          _A2))}
    : 'a mpoly cancel_semigroup_add);;

let rec ab_semigroup_add_mpoly (_A1, _A2) =
  ({semigroup_add_ab_semigroup_add =
      (semigroup_add_mpoly (_A1.monoid_add_comm_monoid_add, _A2))}
    : 'a mpoly ab_semigroup_add);;

let rec minus_mpoly (_A1, _A2) =
  ({minus = minus_mpolya (_A1, _A2)} : 'a mpoly minus);;

let rec cancel_ab_semigroup_add_mpoly (_A1, _A2) =
  ({ab_semigroup_add_cancel_ab_semigroup_add =
      (ab_semigroup_add_mpoly
        (_A1.comm_monoid_add_cancel_comm_monoid_add, _A2));
     cancel_semigroup_add_cancel_ab_semigroup_add =
       (cancel_semigroup_add_mpoly (_A1, _A2));
     minus_cancel_ab_semigroup_add = (minus_mpoly (_A1, _A2))}
    : 'a mpoly cancel_ab_semigroup_add);;

let rec zero_mpoly _A = ({zero = zero_mpolya _A} : 'a mpoly zero);;

let rec monoid_add_mpoly (_A1, _A2) =
  ({semigroup_add_monoid_add = (semigroup_add_mpoly (_A1, _A2));
     zero_monoid_add = (zero_mpoly _A1.zero_monoid_add)}
    : 'a mpoly monoid_add);;

let rec comm_monoid_add_mpoly (_A1, _A2) =
  ({ab_semigroup_add_comm_monoid_add = (ab_semigroup_add_mpoly (_A1, _A2));
     monoid_add_comm_monoid_add =
       (monoid_add_mpoly (_A1.monoid_add_comm_monoid_add, _A2))}
    : 'a mpoly comm_monoid_add);;

let rec cancel_comm_monoid_add_mpoly (_A1, _A2) =
  ({cancel_ab_semigroup_add_cancel_comm_monoid_add =
      (cancel_ab_semigroup_add_mpoly (_A1, _A2));
     comm_monoid_add_cancel_comm_monoid_add =
       (comm_monoid_add_mpoly
         (_A1.comm_monoid_add_cancel_comm_monoid_add, _A2))}
    : 'a mpoly cancel_comm_monoid_add);;

let rec mult_zero_mpoly (_A1, _A2) =
  ({times_mult_zero = (times_mpoly (_A1, _A2));
     zero_mult_zero = (zero_mpoly _A2.mult_zero_semiring_0.zero_mult_zero)}
    : 'a mpoly mult_zero);;

let rec semigroup_mult_mpoly (_A1, _A2) =
  ({times_semigroup_mult = (times_mpoly (_A1, _A2))} :
    'a mpoly semigroup_mult);;

let rec semiring_mpoly (_A1, _A2) =
  ({ab_semigroup_add_semiring =
      (ab_semigroup_add_mpoly (_A2.comm_monoid_add_semiring_0, _A1));
     semigroup_mult_semiring = (semigroup_mult_mpoly (_A1, _A2))}
    : 'a mpoly semiring);;

let rec semiring_0_mpoly (_A1, _A2) =
  ({comm_monoid_add_semiring_0 =
      (comm_monoid_add_mpoly (_A2.comm_monoid_add_semiring_0, _A1));
     mult_zero_semiring_0 = (mult_zero_mpoly (_A1, _A2));
     semiring_semiring_0 = (semiring_mpoly (_A1, _A2))}
    : 'a mpoly semiring_0);;

let rec semiring_0_cancel_mpoly (_A1, _A2) =
  ({cancel_comm_monoid_add_semiring_0_cancel =
      (cancel_comm_monoid_add_mpoly
        (_A2.cancel_comm_monoid_add_semiring_0_cancel, _A1));
     semiring_0_semiring_0_cancel =
       (semiring_0_mpoly (_A1, _A2.semiring_0_semiring_0_cancel))}
    : 'a mpoly semiring_0_cancel);;

let rec ab_semigroup_mult_mpoly (_A1, _A2) =
  ({semigroup_mult_ab_semigroup_mult =
      (semigroup_mult_mpoly (_A1, _A2.semiring_0_comm_semiring_0))}
    : 'a mpoly ab_semigroup_mult);;

let rec comm_semiring_mpoly (_A1, _A2) =
  ({ab_semigroup_mult_comm_semiring = (ab_semigroup_mult_mpoly (_A1, _A2));
     semiring_comm_semiring =
       (semiring_mpoly (_A1, _A2.semiring_0_comm_semiring_0))}
    : 'a mpoly comm_semiring);;

let rec comm_semiring_0_mpoly (_A1, _A2) =
  ({comm_semiring_comm_semiring_0 = (comm_semiring_mpoly (_A1, _A2));
     semiring_0_comm_semiring_0 =
       (semiring_0_mpoly (_A1, _A2.semiring_0_comm_semiring_0))}
    : 'a mpoly comm_semiring_0);;

let rec comm_semiring_0_cancel_mpoly (_A1, _A2) =
  ({comm_semiring_0_comm_semiring_0_cancel =
      (comm_semiring_0_mpoly (_A1, _A2.comm_semiring_0_comm_semiring_0_cancel));
     semiring_0_cancel_comm_semiring_0_cancel =
       (semiring_0_cancel_mpoly
         (_A1, _A2.semiring_0_cancel_comm_semiring_0_cancel))}
    : 'a mpoly comm_semiring_0_cancel);;

let rec power_mpoly (_A1, _A2) =
  ({one_power = (one_mpoly _A2.zero_neq_one_semiring_1);
     times_power = (times_mpoly (_A1, _A2.semiring_0_semiring_1))}
    : 'a mpoly power);;

let rec monoid_mult_mpoly (_A1, _A2) =
  ({semigroup_mult_monoid_mult =
      (semigroup_mult_mpoly (_A1, _A2.semiring_0_semiring_1));
     power_monoid_mult = (power_mpoly (_A1, _A2))}
    : 'a mpoly monoid_mult);;

let rec numeral_mpoly (_A1, _A2) =
  ({one_numeral = (one_mpoly _A2.zero_neq_one_semiring_1);
     semigroup_add_numeral =
       (semigroup_add_mpoly
         (_A2.semiring_0_semiring_1.comm_monoid_add_semiring_0.monoid_add_comm_monoid_add,
           _A1))}
    : 'a mpoly numeral);;

let rec semiring_numeral_mpoly (_A1, _A2) =
  ({monoid_mult_semiring_numeral = (monoid_mult_mpoly (_A1, _A2));
     numeral_semiring_numeral = (numeral_mpoly (_A1, _A2));
     semiring_semiring_numeral =
       (semiring_mpoly (_A1, _A2.semiring_0_semiring_1))}
    : 'a mpoly semiring_numeral);;

let rec zero_neq_one_mpoly _A =
  ({one_zero_neq_one = (one_mpoly _A);
     zero_zero_neq_one = (zero_mpoly _A.zero_zero_neq_one)}
    : 'a mpoly zero_neq_one);;

let rec semiring_1_mpoly (_A1, _A2) =
  ({semiring_numeral_semiring_1 = (semiring_numeral_mpoly (_A1, _A2));
     semiring_0_semiring_1 =
       (semiring_0_mpoly (_A1, _A2.semiring_0_semiring_1));
     zero_neq_one_semiring_1 = (zero_neq_one_mpoly _A2.zero_neq_one_semiring_1)}
    : 'a mpoly semiring_1);;

let rec semiring_1_cancel_mpoly (_A1, _A2) =
  ({semiring_0_cancel_semiring_1_cancel =
      (semiring_0_cancel_mpoly (_A1, _A2.semiring_0_cancel_semiring_1_cancel));
     semiring_1_semiring_1_cancel =
       (semiring_1_mpoly (_A1, _A2.semiring_1_semiring_1_cancel))}
    : 'a mpoly semiring_1_cancel);;

let rec comm_monoid_mult_mpoly (_A1, _A2) =
  ({ab_semigroup_mult_comm_monoid_mult =
      (ab_semigroup_mult_mpoly (_A1, _A2.comm_semiring_0_comm_semiring_1));
     monoid_mult_comm_monoid_mult =
       (monoid_mult_mpoly (_A1, _A2.semiring_1_comm_semiring_1));
     dvd_comm_monoid_mult = (dvd_mpoly (_A1, _A2))}
    : 'a mpoly comm_monoid_mult);;

let rec comm_semiring_1_mpoly (_A1, _A2) =
  ({comm_monoid_mult_comm_semiring_1 = (comm_monoid_mult_mpoly (_A1, _A2));
     comm_semiring_0_comm_semiring_1 =
       (comm_semiring_0_mpoly (_A1, _A2.comm_semiring_0_comm_semiring_1));
     semiring_1_comm_semiring_1 =
       (semiring_1_mpoly (_A1, _A2.semiring_1_comm_semiring_1))}
    : 'a mpoly comm_semiring_1);;

let rec comm_semiring_1_cancel_mpoly (_A1, _A2) =
  ({comm_semiring_0_cancel_comm_semiring_1_cancel =
      (comm_semiring_0_cancel_mpoly
        (_A1, _A2.comm_ring_comm_ring_1.comm_semiring_0_cancel_comm_ring));
     comm_semiring_1_comm_semiring_1_cancel =
       (comm_semiring_1_mpoly
         (_A1, _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel));
     semiring_1_cancel_comm_semiring_1_cancel =
       (semiring_1_cancel_mpoly
         (_A1, _A2.ring_1_comm_ring_1.semiring_1_cancel_ring_1))}
    : 'a mpoly comm_semiring_1_cancel);;

let rec comm_semiring_1_cancel_crossproduct_mpoly (_A1, _A2) =
  ({comm_semiring_1_cancel_comm_semiring_1_cancel_crossproduct =
      (comm_semiring_1_cancel_mpoly (_A1, _A2.comm_ring_1_idom))}
    : 'a mpoly comm_semiring_1_cancel_crossproduct);;

let rec semiring_no_zero_divisors_mpoly (_A1, _A2) =
  ({semiring_0_semiring_no_zero_divisors =
      (semiring_0_mpoly
        (_A1, _A2.ring_ring_no_zero_divisors.semiring_0_cancel_ring.semiring_0_semiring_0_cancel))}
    : 'a mpoly semiring_no_zero_divisors);;

let rec semiring_1_no_zero_divisors_mpoly (_A1, _A2) =
  ({semiring_1_semiring_1_no_zero_divisors =
      (semiring_1_mpoly
        (_A1, _A2.ring_1_ring_1_no_zero_divisors.semiring_1_cancel_ring_1.semiring_1_semiring_1_cancel));
     semiring_no_zero_divisors_semiring_1_no_zero_divisors =
       (semiring_no_zero_divisors_mpoly
         (_A1, _A2.ring_no_zero_divisors_ring_1_no_zero_divisors))}
    : 'a mpoly semiring_1_no_zero_divisors);;

let rec semiring_no_zero_divisors_cancel_mpoly (_A1, _A2) =
  ({semiring_no_zero_divisors_semiring_no_zero_divisors_cancel =
      (semiring_no_zero_divisors_mpoly (_A1, _A2))}
    : 'a mpoly semiring_no_zero_divisors_cancel);;

let rec uminus_mpoly _A = ({uminus = uminus_mpolya _A} : 'a mpoly uminus);;

let rec group_add_mpoly (_A1, _A2) =
  ({cancel_semigroup_add_group_add =
      (cancel_semigroup_add_mpoly
        (_A1.cancel_comm_monoid_add_ab_group_add, _A2));
     minus_group_add =
       (minus_mpoly (_A1.cancel_comm_monoid_add_ab_group_add, _A2));
     monoid_add_group_add =
       (monoid_add_mpoly
         (_A1.group_add_ab_group_add.monoid_add_group_add, _A2));
     uminus_group_add = (uminus_mpoly _A1)}
    : 'a mpoly group_add);;

let rec ab_group_add_mpoly (_A1, _A2) =
  ({cancel_comm_monoid_add_ab_group_add =
      (cancel_comm_monoid_add_mpoly
        (_A1.cancel_comm_monoid_add_ab_group_add, _A2));
     group_add_ab_group_add = (group_add_mpoly (_A1, _A2))}
    : 'a mpoly ab_group_add);;

let rec ring_mpoly (_A1, _A2) =
  ({ab_group_add_ring = (ab_group_add_mpoly (_A2.ab_group_add_ring, _A1));
     semiring_0_cancel_ring =
       (semiring_0_cancel_mpoly (_A1, _A2.semiring_0_cancel_ring))}
    : 'a mpoly ring);;

let rec ring_no_zero_divisors_mpoly (_A1, _A2) =
  ({ring_ring_no_zero_divisors =
      (ring_mpoly (_A1, _A2.ring_ring_no_zero_divisors));
     semiring_no_zero_divisors_cancel_ring_no_zero_divisors =
       (semiring_no_zero_divisors_cancel_mpoly (_A1, _A2))}
    : 'a mpoly ring_no_zero_divisors);;

let rec neg_numeral_mpoly (_A1, _A2) =
  ({group_add_neg_numeral =
      (group_add_mpoly (_A2.ring_ring_1.ab_group_add_ring, _A1));
     numeral_neg_numeral =
       (numeral_mpoly
         (_A1, _A2.semiring_1_cancel_ring_1.semiring_1_semiring_1_cancel))}
    : 'a mpoly neg_numeral);;

let rec ring_1_mpoly (_A1, _A2) =
  ({neg_numeral_ring_1 = (neg_numeral_mpoly (_A1, _A2));
     ring_ring_1 = (ring_mpoly (_A1, _A2.ring_ring_1));
     semiring_1_cancel_ring_1 =
       (semiring_1_cancel_mpoly (_A1, _A2.semiring_1_cancel_ring_1))}
    : 'a mpoly ring_1);;

let rec ring_1_no_zero_divisors_mpoly (_A1, _A2) =
  ({ring_1_ring_1_no_zero_divisors =
      (ring_1_mpoly (_A1, _A2.ring_1_ring_1_no_zero_divisors));
     ring_no_zero_divisors_ring_1_no_zero_divisors =
       (ring_no_zero_divisors_mpoly
         (_A1, _A2.ring_no_zero_divisors_ring_1_no_zero_divisors));
     semiring_1_no_zero_divisors_ring_1_no_zero_divisors =
       (semiring_1_no_zero_divisors_mpoly (_A1, _A2))}
    : 'a mpoly ring_1_no_zero_divisors);;

let rec comm_ring_mpoly (_A1, _A2) =
  ({comm_semiring_0_cancel_comm_ring =
      (comm_semiring_0_cancel_mpoly
        (_A1, _A2.comm_semiring_0_cancel_comm_ring));
     ring_comm_ring = (ring_mpoly (_A1, _A2.ring_comm_ring))}
    : 'a mpoly comm_ring);;

let rec comm_ring_1_mpoly (_A1, _A2) =
  ({comm_ring_comm_ring_1 = (comm_ring_mpoly (_A1, _A2.comm_ring_comm_ring_1));
     comm_semiring_1_cancel_comm_ring_1 =
       (comm_semiring_1_cancel_mpoly (_A1, _A2));
     ring_1_comm_ring_1 = (ring_1_mpoly (_A1, _A2.ring_1_comm_ring_1))}
    : 'a mpoly comm_ring_1);;

let rec semidom_mpoly (_A1, _A2) =
  ({comm_semiring_1_cancel_semidom =
      (comm_semiring_1_cancel_mpoly (_A1, _A2.comm_ring_1_idom));
     semiring_1_no_zero_divisors_semidom =
       (semiring_1_no_zero_divisors_mpoly
         (_A1, _A2.ring_1_no_zero_divisors_idom))}
    : 'a mpoly semidom);;

let rec idom_mpoly (_A1, _A2) =
  ({comm_ring_1_idom = (comm_ring_1_mpoly (_A1, _A2.comm_ring_1_idom));
     ring_1_no_zero_divisors_idom =
       (ring_1_no_zero_divisors_mpoly (_A1, _A2.ring_1_no_zero_divisors_idom));
     semidom_idom = (semidom_mpoly (_A1, _A2));
     comm_semiring_1_cancel_crossproduct_idom =
       (comm_semiring_1_cancel_crossproduct_mpoly (_A1, _A2))}
    : 'a mpoly idom);;

let rec lookupa _A _B (Pm_fmap xs) x = fmlookup_default _A (zero _B) xs x;;

let rec filter_pm _B p (Pm_fmap m) = Pm_fmap (fmfilter p m);;

let rec remove_key _A _B
  x m = filter_pm _B.zero_monoid_add (fun y -> not (eq _A y x)) m;;

let rec mpoly_to_mpoly_poly_impl_aux1
  i x1 j = match i, x1, j with i, [], j -> []
    | i, (mon, c) :: xs, j ->
        (if equal_nata (lookupa equal_nat zero_nat mon i) j
          then [(remove_key equal_nat monoid_add_nat i mon, c)] else []) @
          mpoly_to_mpoly_poly_impl_aux1 i xs j;;

let rec mpoly_to_mpoly_poly_impl_aux2 _A
  i (MPoly (Pm_fmap (Fmap_of_list xs))) j =
    MPoly (Pm_fmap (Fmap_of_list (mpoly_to_mpoly_poly_impl_aux1 i xs j)));;

type 'a semilattice_set = Abs_semilattice_set of ('a -> 'a -> 'a);;

let rec semilattice_set_apply (Abs_semilattice_set x) = x;;

let rec is_empty _A
  xa = (match impl_of _A xa with Empty -> true
         | Branch (_, _, _, _, _) -> false);;

let rec rBT_Impl_fold1
  f x1 = match f, x1 with
    f, Branch (ca, Branch (c, l, ka, va, ra), k, v, r) ->
      folda (fun kb _ -> f kb) r
        (f k (rBT_Impl_fold1 f (Branch (c, l, ka, va, ra))))
    | f, Branch (c, Empty, k, v, r) -> folda (fun ka _ -> f ka) r k
    | f, Empty -> failwith "undefined";;

let rec fold1 _A x xc = rBT_Impl_fold1 x (impl_of _A xc);;

let rec nulla _A xa = null (list_of_dlist _A xa);;

let rec tl _A xa = Abs_dlist (tla (list_of_dlist _A xa));;

let rec hd _A xa = hda (list_of_dlist _A xa);;

let rec set_fold1 (_A1, _A2, _A3)
  f x1 = match f, x1 with
    f, RBT_set rbt ->
      (match ccompare _A2
        with None ->
          failwith "set_fold1 RBT_set: ccompare = None"
            (fun _ -> set_fold1 (_A1, _A2, _A3) f (RBT_set rbt))
        | Some _ ->
          (if is_empty _A2 rbt
            then failwith "set_fold1 RBT_set: empty set"
                   (fun _ -> set_fold1 (_A1, _A2, _A3) f (RBT_set rbt))
            else fold1 _A2 (semilattice_set_apply f) rbt))
    | f, DList_set dxs ->
        (match ceq _A1
          with None ->
            failwith "set_fold1 DList_set: ceq = None"
              (fun _ -> set_fold1 (_A1, _A2, _A3) f (DList_set dxs))
          | Some _ ->
            (if nulla _A1 dxs
              then failwith "set_fold1 DList_set: empty set"
                     (fun _ -> set_fold1 (_A1, _A2, _A3) f (DList_set dxs))
              else foldc _A1 (semilattice_set_apply f) (tl _A1 dxs)
                     (hd _A1 dxs)))
    | f, Set_Monad (x :: xs) -> fold (semilattice_set_apply f) xs x
    | f, Collect_set p ->
        failwith "set_fold1: Collect_set"
          (fun _ -> set_fold1 (_A1, _A2, _A3) f (Collect_set p))
    | f, Complement a ->
        failwith "set_fold1: Complement"
          (fun _ -> set_fold1 (_A1, _A2, _A3) f (Complement a));;

let rec max_sls _A
  = Abs_semilattice_set (max _A.order_linorder.preorder_order.ord_preorder);;

let rec maxa (_A1, _A2, _A3, _A4)
  a = set_fold1 (_A1, _A2, _A3) (max_sls _A4) a;;

let rec fmdoma (_A1, _A2, _A3) (_B1, _B2, _B3)
  m = fset (fmdom (_A1, _A2, _A3) (_B1, _B2, _B3) m);;

let rec keysb (_A1, _A2, _A3, _A4) (_B1, _B2, _B3, _B4, _B5)
  (Pm_fmap xs) =
    fmdoma (_A1, _A2, _A4) (_B1, _B2, _B5) (clearjunk0 _A3 (_B3, _B4) xs);;

let rec degree (_A1, _A2, _A3, _A4, _A5)
  (MPoly xa) =
    (fun v ->
      maxa (ceq_nat, ccompare_nat, lattice_nat, linorder_nat)
        (insert (ceq_nat, ccompare_nat) zero_nata
          (image
            ((ceq_poly_mapping (ceq_nat, ccompare_nat, equal_nat, set_impl_nat)
               (ceq_nat, ccompare_nat, zero_nat, equal_nat, set_impl_nat)),
              ccompare_poly_mapping)
            (ceq_nat, ccompare_nat, set_impl_nat)
            (fun m -> lookupa equal_nat zero_nat m v)
            (keysb
              ((ceq_poly_mapping
                 (ceq_nat, ccompare_nat, equal_nat, set_impl_nat)
                 (ceq_nat, ccompare_nat, zero_nat, equal_nat, set_impl_nat)),
                ccompare_poly_mapping,
                (equal_poly_mapping
                  (ceq_nat, ccompare_nat, equal_nat, set_impl_nat)
                  (ceq_nat, ccompare_nat, zero_nat, equal_nat, set_impl_nat)),
                set_impl_poly_mapping)
              (_A1, _A2, _A3, _A4, _A5) xa))));;

let rec mpoly_to_mpoly_poly_impl (_A1, _A2, _A3, _A4, _A5)
  x p = (if eq (equal_mpoly
                 (_A1, _A2,
                   _A4.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
                   _A3, _A5))
              p (zero_mpolya
                  _A4.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
          then []
          else map (mpoly_to_mpoly_poly_impl_aux2 _A4 x p)
                 (upt zero_nata
                   (suc (degree
                          (_A1, _A2,
                            _A4.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
                            _A3, _A5)
                          p x))));;

let rec mpoly_to_mpoly_poly (_A1, _A2, _A3, _A4, _A5)
  x p = Poly (mpoly_to_mpoly_poly_impl (_A1, _A2, _A3, _A4, _A5) x p);;

type ('b, 'a) comp_fun_idem = Abs_comp_fun_idem of ('b -> 'a -> 'a);;

let rec comp_fun_idem_apply (Abs_comp_fun_idem x) = x;;

let rec set_fold_cfi (_A1, _A2)
  f b x2 = match f, b, x2 with
    f, b, RBT_set rbt ->
      (match ccompare _A2
        with None ->
          failwith "set_fold_cfi RBT_set: ccompare = None"
            (fun _ -> set_fold_cfi (_A1, _A2) f b (RBT_set rbt))
        | Some _ -> foldb _A2 (comp_fun_idem_apply f) rbt b)
    | f, b, DList_set dxs ->
        (match ceq _A1
          with None ->
            failwith "set_fold_cfi DList_set: ceq = None"
              (fun _ -> set_fold_cfi (_A1, _A2) f b (DList_set dxs))
          | Some _ -> foldc _A1 (comp_fun_idem_apply f) dxs b)
    | f, b, Set_Monad xs -> fold (comp_fun_idem_apply f) xs b
    | f, b, Collect_set p ->
        failwith "set_fold_cfi not supported on Collect_set"
          (fun _ -> set_fold_cfi (_A1, _A2) f b (Collect_set p))
    | f, b, Complement a ->
        failwith "set_fold_cfi not supported on Complement"
          (fun _ -> set_fold_cfi (_A1, _A2) f b (Complement a));;

let rec sup_cfi _A
  = Abs_comp_fun_idem (sup _A.semilattice_sup_lattice.sup_semilattice_sup);;

let rec sup_setb (_A1, _A2, _A3, _A4, _A5)
  a = (if finite
            ((finite_UNIV_set _A1),
              (ceq_set (_A2, _A3, _A4.ccompare_cproper_interval)),
              (ccompare_set (_A1, _A3, _A4, _A5)))
            a
        then set_fold_cfi
               ((ceq_set (_A2, _A3, _A4.ccompare_cproper_interval)),
                 (ccompare_set (_A1, _A3, _A4, _A5)))
               (sup_cfi (lattice_set (_A2, _A3, _A4.ccompare_cproper_interval)))
               (bot_set (_A3, _A4.ccompare_cproper_interval, _A5)) a
        else failwith "Sup: infinite"
               (fun _ -> sup_setb (_A1, _A2, _A3, _A4, _A5) a));;

let rec mapping_of _A (MPoly p) = p;;

let rec vars (_A1, _A2, _A3, _A4, _A5)
  p = sup_setb
        (finite_UNIV_nat, cenum_nat, ceq_nat, cproper_interval_nat,
          set_impl_nat)
        (image
          ((ceq_poly_mapping (ceq_nat, ccompare_nat, equal_nat, set_impl_nat)
             (ceq_nat, ccompare_nat, zero_nat, equal_nat, set_impl_nat)),
            ccompare_poly_mapping)
          ((ceq_set
             (cenum_nat, ceq_nat,
               cproper_interval_nat.ccompare_cproper_interval)),
            (ccompare_set
              (finite_UNIV_nat, ceq_nat, cproper_interval_nat, set_impl_nat)),
            set_impl_set)
          (keysb (ceq_nat, ccompare_nat, equal_nat, set_impl_nat)
            (ceq_nat, ccompare_nat, zero_nat, equal_nat, set_impl_nat))
          (keysb
            ((ceq_poly_mapping (ceq_nat, ccompare_nat, equal_nat, set_impl_nat)
               (ceq_nat, ccompare_nat, zero_nat, equal_nat, set_impl_nat)),
              ccompare_poly_mapping,
              (equal_poly_mapping
                (ceq_nat, ccompare_nat, equal_nat, set_impl_nat)
                (ceq_nat, ccompare_nat, zero_nat, equal_nat, set_impl_nat)),
              set_impl_poly_mapping)
            (_A1, _A2, _A3, _A4, _A5) (mapping_of _A3 p)));;

let rec coeff _A
  p = lookupa
        (equal_poly_mapping (ceq_nat, ccompare_nat, equal_nat, set_impl_nat)
          (ceq_nat, ccompare_nat, zero_nat, equal_nat, set_impl_nat))
        _A (mapping_of _A p);;

let rec single (_B1, _B2)
  t c = (if eq _B2 c (zero _B1) then zero_poly_mappinga _B1
          else sparse_0 _B1 [(t, c)]);;

let rec const_0 (_A1, _A2)
  c = single (_A1, _A2) (zero_poly_mappinga zero_nat) c;;

let rec const (_A1, _A2) x = MPoly (const_0 (_A1, _A2) x);;

let rec var_0 (_B1, _B2, _B3)
  n = single (_B2, _B3) (single (zero_nat, equal_nat) n one_nata) (one _B1);;

let rec var (_A1, _A2, _A3) x = MPoly (var_0 (_A1, _A2, _A3) x);;

let rec divide_option_mpoly (_A1, _A2, _A3, _A4, _A5)
  p q = (let v =
           sup_seta (ceq_nat, ccompare_nat)
             (vars (_A1, _A2,
                     _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
                     _A3, _A5)
               p)
             (vars (_A1, _A2,
                     _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
                     _A3, _A5)
               q)
           in
          (if set_eq (cenum_nat, ceq_nat, ccompare_nat) v
                (set_empty (ceq_nat, ccompare_nat) (of_phantom set_impl_nata))
            then (let a =
                    coeff _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
                      p (zero_poly_mappinga zero_nat)
                    in
                  let b =
                    coeff _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
                      q (zero_poly_mappinga zero_nat)
                    in
                  let c =
                    divide _A4.semidom_divide_idom_divide.divide_semidom_divide
                      a b
                    in
                   (if eq _A3
                         (times
                           _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd
                           b c)
                         a
                     then Some (const
                                 (_A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
                                   _A3)
                                 c)
                     else None))
            else (let x =
                    maxa (ceq_nat, ccompare_nat, lattice_nat, linorder_nat) v in
                  let pa =
                    mpoly_to_mpoly_poly
                      (_A1, _A2, _A3, _A4.idom_idom_divide.comm_ring_1_idom,
                        _A5)
                      x p
                    in
                  let qa =
                    mpoly_to_mpoly_poly
                      (_A1, _A2, _A3, _A4.idom_idom_divide.comm_ring_1_idom,
                        _A5)
                      x q
                    in
                   (match
                     divide_option_mpoly_poly (_A1, _A2, _A3, _A4, _A5) pa qa
                     with None -> None
                     | Some r ->
                       Some (poly (comm_semiring_0_mpoly
                                    (_A3, _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_semiring_0_comm_semiring_1))
                              r (var (_A4.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.numeral_neg_numeral.one_numeral,
                                       _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
                                       _A3)
                                  x))))))
and divide_option_mpoly_poly (_A1, _A2, _A3, _A4, _A5)
  p q = (if is_zero
              (zero_mpoly
                _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
              p
          then Some (zero_polya
                      (zero_mpoly
                        _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero))
          else (if is_zero
                     (zero_mpoly
                       _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
                     q
                 then None
                 else (let dp =
                         degreea
                           (zero_mpoly
                             _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
                           p
                         in
                       let dq =
                         degreea
                           (zero_mpoly
                             _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
                           q
                         in
                        (if less_nat dp dq then None
                          else (match
                                 divide_option_mpoly (_A1, _A2, _A3, _A4, _A5)
                                   (coeffa
                                     (zero_mpoly
                                       _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
                                     p (degreea
 (zero_mpoly
   _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
 p))
                                   (coeffa
                                     (zero_mpoly
                                       _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
                                     q (degreea
 (zero_mpoly
   _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
 q))
                                 with None -> None
                                 | Some c ->
                                   (match
                                     divide_option_mpoly_poly
                                       (_A1, _A2, _A3, _A4, _A5)
                                       (minus_polya
 ((ab_group_add_mpoly
    (_A4.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.ring_ring_1.ab_group_add_ring,
      _A3)),
   (equal_mpoly
     (_A1, _A2,
       _A4.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.ring_ring_1.ab_group_add_ring.group_add_ab_group_add.monoid_add_group_add.zero_monoid_add,
       _A3, _A5)))
 p (karatsuba_mult_poly
     ((equal_mpoly
        (_A1, _A2,
          _A4.idom_idom_divide.comm_ring_1_idom.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
          _A3, _A5)),
       (comm_ring_1_mpoly (_A3, _A4.idom_idom_divide.comm_ring_1_idom)),
       (semiring_no_zero_divisors_mpoly
         (_A3, _A4.idom_idom_divide.ring_1_no_zero_divisors_idom.ring_no_zero_divisors_ring_1_no_zero_divisors)))
     (monoma
       ((zero_mpoly
          _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero),
         (equal_mpoly
           (_A1, _A2,
             _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
             _A3, _A5)))
       c (minus_nata dp dq))
     q))
                                       q
                                     with None -> None
                                     | Some r ->
                                       Some (plus_polya
      ((comm_monoid_add_mpoly
         (_A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.comm_monoid_add_semiring_0,
           _A3)),
        (equal_mpoly
          (_A1, _A2,
            _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.comm_monoid_add_semiring_0.monoid_add_comm_monoid_add.zero_monoid_add,
            _A3, _A5)))
      (monoma
        ((zero_mpoly
           _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero),
          (equal_mpoly
            (_A1, _A2,
              _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
              _A3, _A5)))
        c (minus_nata dp dq))
      r)))))));;

let rec divide_mpolya (_A1, _A2, _A3, _A4, _A5)
  p q = (match divide_option_mpoly (_A1, _A2, _A3, _A4, _A5) p q
          with None ->
            zero_mpolya
              _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
          | Some r -> r);;

let rec divide_mpoly (_A1, _A2, _A3, _A4, _A5) =
  ({divide = divide_mpolya (_A1, _A2, _A3, _A4, _A5)} : 'a mpoly divide);;

let rec semidom_divide_mpoly (_A1, _A2, _A3, _A4, _A5) =
  ({divide_semidom_divide = (divide_mpoly (_A1, _A2, _A3, _A4, _A5));
     semidom_semidom_divide = (semidom_mpoly (_A3, _A4.idom_idom_divide));
     semiring_no_zero_divisors_cancel_semidom_divide =
       (semiring_no_zero_divisors_cancel_mpoly
         (_A3, _A4.idom_idom_divide.ring_1_no_zero_divisors_idom.ring_no_zero_divisors_ring_1_no_zero_divisors))}
    : 'a mpoly semidom_divide);;

let rec idom_divide_mpoly (_A1, _A2, _A3, _A4, _A5) =
  ({idom_idom_divide = (idom_mpoly (_A3, _A4.idom_idom_divide));
     semidom_divide_idom_divide =
       (semidom_divide_mpoly (_A1, _A2, _A3, _A4, _A5))}
    : 'a mpoly idom_divide);;

let rec less_eq_prod _A _B
  (x1, y1) (x2, y2) = less _A x1 x2 || less_eq _A x1 x2 && less_eq _B y1 y2;;

let rec less_prod _A _B
  (x1, y1) (x2, y2) = less _A x1 x2 || less_eq _A x1 x2 && less _B y1 y2;;

let rec ord_prod _A _B =
  ({less_eq = less_eq_prod _A _B; less = less_prod _A _B} : ('a * 'b) ord);;

let rec preorder_prod _A _B =
  ({ord_preorder = (ord_prod _A.ord_preorder _B.ord_preorder)} :
    ('a * 'b) preorder);;

let rec order_prod _A _B =
  ({preorder_order = (preorder_prod _A.preorder_order _B.preorder_order)} :
    ('a * 'b) order);;

let rec linorder_prod _A _B =
  ({order_linorder = (order_prod _A.order_linorder _B.order_linorder)} :
    ('a * 'b) linorder);;

let times_integer = ({times = Z.mul} : Z.t times);;

let dvd_integer = ({times_dvd = times_integer} : Z.t dvd);;

let one_integera : Z.t = (Z.of_int 1);;

let one_integer = ({one = one_integera} : Z.t one);;

let plus_integer = ({plus = Z.add} : Z.t plus);;

let semigroup_add_integer =
  ({plus_semigroup_add = plus_integer} : Z.t semigroup_add);;

let cancel_semigroup_add_integer =
  ({semigroup_add_cancel_semigroup_add = semigroup_add_integer} :
    Z.t cancel_semigroup_add);;

let ab_semigroup_add_integer =
  ({semigroup_add_ab_semigroup_add = semigroup_add_integer} :
    Z.t ab_semigroup_add);;

let minus_integer = ({minus = Z.sub} : Z.t minus);;

let cancel_ab_semigroup_add_integer =
  ({ab_semigroup_add_cancel_ab_semigroup_add = ab_semigroup_add_integer;
     cancel_semigroup_add_cancel_ab_semigroup_add =
       cancel_semigroup_add_integer;
     minus_cancel_ab_semigroup_add = minus_integer}
    : Z.t cancel_ab_semigroup_add);;

let zero_integer = ({zero = Z.zero} : Z.t zero);;

let monoid_add_integer =
  ({semigroup_add_monoid_add = semigroup_add_integer;
     zero_monoid_add = zero_integer}
    : Z.t monoid_add);;

let comm_monoid_add_integer =
  ({ab_semigroup_add_comm_monoid_add = ab_semigroup_add_integer;
     monoid_add_comm_monoid_add = monoid_add_integer}
    : Z.t comm_monoid_add);;

let cancel_comm_monoid_add_integer =
  ({cancel_ab_semigroup_add_cancel_comm_monoid_add =
      cancel_ab_semigroup_add_integer;
     comm_monoid_add_cancel_comm_monoid_add = comm_monoid_add_integer}
    : Z.t cancel_comm_monoid_add);;

let mult_zero_integer =
  ({times_mult_zero = times_integer; zero_mult_zero = zero_integer} :
    Z.t mult_zero);;

let semigroup_mult_integer =
  ({times_semigroup_mult = times_integer} : Z.t semigroup_mult);;

let semiring_integer =
  ({ab_semigroup_add_semiring = ab_semigroup_add_integer;
     semigroup_mult_semiring = semigroup_mult_integer}
    : Z.t semiring);;

let semiring_0_integer =
  ({comm_monoid_add_semiring_0 = comm_monoid_add_integer;
     mult_zero_semiring_0 = mult_zero_integer;
     semiring_semiring_0 = semiring_integer}
    : Z.t semiring_0);;

let semiring_0_cancel_integer =
  ({cancel_comm_monoid_add_semiring_0_cancel = cancel_comm_monoid_add_integer;
     semiring_0_semiring_0_cancel = semiring_0_integer}
    : Z.t semiring_0_cancel);;

let ab_semigroup_mult_integer =
  ({semigroup_mult_ab_semigroup_mult = semigroup_mult_integer} :
    Z.t ab_semigroup_mult);;

let comm_semiring_integer =
  ({ab_semigroup_mult_comm_semiring = ab_semigroup_mult_integer;
     semiring_comm_semiring = semiring_integer}
    : Z.t comm_semiring);;

let comm_semiring_0_integer =
  ({comm_semiring_comm_semiring_0 = comm_semiring_integer;
     semiring_0_comm_semiring_0 = semiring_0_integer}
    : Z.t comm_semiring_0);;

let comm_semiring_0_cancel_integer =
  ({comm_semiring_0_comm_semiring_0_cancel = comm_semiring_0_integer;
     semiring_0_cancel_comm_semiring_0_cancel = semiring_0_cancel_integer}
    : Z.t comm_semiring_0_cancel);;

let power_integer =
  ({one_power = one_integer; times_power = times_integer} : Z.t power);;

let monoid_mult_integer =
  ({semigroup_mult_monoid_mult = semigroup_mult_integer;
     power_monoid_mult = power_integer}
    : Z.t monoid_mult);;

let numeral_integer =
  ({one_numeral = one_integer; semigroup_add_numeral = semigroup_add_integer} :
    Z.t numeral);;

let semiring_numeral_integer =
  ({monoid_mult_semiring_numeral = monoid_mult_integer;
     numeral_semiring_numeral = numeral_integer;
     semiring_semiring_numeral = semiring_integer}
    : Z.t semiring_numeral);;

let zero_neq_one_integer =
  ({one_zero_neq_one = one_integer; zero_zero_neq_one = zero_integer} :
    Z.t zero_neq_one);;

let semiring_1_integer =
  ({semiring_numeral_semiring_1 = semiring_numeral_integer;
     semiring_0_semiring_1 = semiring_0_integer;
     zero_neq_one_semiring_1 = zero_neq_one_integer}
    : Z.t semiring_1);;

let semiring_1_cancel_integer =
  ({semiring_0_cancel_semiring_1_cancel = semiring_0_cancel_integer;
     semiring_1_semiring_1_cancel = semiring_1_integer}
    : Z.t semiring_1_cancel);;

let comm_monoid_mult_integer =
  ({ab_semigroup_mult_comm_monoid_mult = ab_semigroup_mult_integer;
     monoid_mult_comm_monoid_mult = monoid_mult_integer;
     dvd_comm_monoid_mult = dvd_integer}
    : Z.t comm_monoid_mult);;

let comm_semiring_1_integer =
  ({comm_monoid_mult_comm_semiring_1 = comm_monoid_mult_integer;
     comm_semiring_0_comm_semiring_1 = comm_semiring_0_integer;
     semiring_1_comm_semiring_1 = semiring_1_integer}
    : Z.t comm_semiring_1);;

let comm_semiring_1_cancel_integer =
  ({comm_semiring_0_cancel_comm_semiring_1_cancel =
      comm_semiring_0_cancel_integer;
     comm_semiring_1_comm_semiring_1_cancel = comm_semiring_1_integer;
     semiring_1_cancel_comm_semiring_1_cancel = semiring_1_cancel_integer}
    : Z.t comm_semiring_1_cancel);;

let comm_semiring_1_cancel_crossproduct_integer =
  ({comm_semiring_1_cancel_comm_semiring_1_cancel_crossproduct =
      comm_semiring_1_cancel_integer}
    : Z.t comm_semiring_1_cancel_crossproduct);;

let semiring_no_zero_divisors_integer =
  ({semiring_0_semiring_no_zero_divisors = semiring_0_integer} :
    Z.t semiring_no_zero_divisors);;

let semiring_1_no_zero_divisors_integer =
  ({semiring_1_semiring_1_no_zero_divisors = semiring_1_integer;
     semiring_no_zero_divisors_semiring_1_no_zero_divisors =
       semiring_no_zero_divisors_integer}
    : Z.t semiring_1_no_zero_divisors);;

let semiring_no_zero_divisors_cancel_integer =
  ({semiring_no_zero_divisors_semiring_no_zero_divisors_cancel =
      semiring_no_zero_divisors_integer}
    : Z.t semiring_no_zero_divisors_cancel);;

let uminus_integer = ({uminus = Z.neg} : Z.t uminus);;

let group_add_integer =
  ({cancel_semigroup_add_group_add = cancel_semigroup_add_integer;
     minus_group_add = minus_integer; monoid_add_group_add = monoid_add_integer;
     uminus_group_add = uminus_integer}
    : Z.t group_add);;

let ab_group_add_integer =
  ({cancel_comm_monoid_add_ab_group_add = cancel_comm_monoid_add_integer;
     group_add_ab_group_add = group_add_integer}
    : Z.t ab_group_add);;

let ring_integer =
  ({ab_group_add_ring = ab_group_add_integer;
     semiring_0_cancel_ring = semiring_0_cancel_integer}
    : Z.t ring);;

let ring_no_zero_divisors_integer =
  ({ring_ring_no_zero_divisors = ring_integer;
     semiring_no_zero_divisors_cancel_ring_no_zero_divisors =
       semiring_no_zero_divisors_cancel_integer}
    : Z.t ring_no_zero_divisors);;

let neg_numeral_integer =
  ({group_add_neg_numeral = group_add_integer;
     numeral_neg_numeral = numeral_integer}
    : Z.t neg_numeral);;

let ring_1_integer =
  ({neg_numeral_ring_1 = neg_numeral_integer; ring_ring_1 = ring_integer;
     semiring_1_cancel_ring_1 = semiring_1_cancel_integer}
    : Z.t ring_1);;

let ring_1_no_zero_divisors_integer =
  ({ring_1_ring_1_no_zero_divisors = ring_1_integer;
     ring_no_zero_divisors_ring_1_no_zero_divisors =
       ring_no_zero_divisors_integer;
     semiring_1_no_zero_divisors_ring_1_no_zero_divisors =
       semiring_1_no_zero_divisors_integer}
    : Z.t ring_1_no_zero_divisors);;

let comm_ring_integer =
  ({comm_semiring_0_cancel_comm_ring = comm_semiring_0_cancel_integer;
     ring_comm_ring = ring_integer}
    : Z.t comm_ring);;

let comm_ring_1_integer =
  ({comm_ring_comm_ring_1 = comm_ring_integer;
     comm_semiring_1_cancel_comm_ring_1 = comm_semiring_1_cancel_integer;
     ring_1_comm_ring_1 = ring_1_integer}
    : Z.t comm_ring_1);;

let semidom_integer =
  ({comm_semiring_1_cancel_semidom = comm_semiring_1_cancel_integer;
     semiring_1_no_zero_divisors_semidom = semiring_1_no_zero_divisors_integer}
    : Z.t semidom);;

let idom_integer =
  ({comm_ring_1_idom = comm_ring_1_integer;
     ring_1_no_zero_divisors_idom = ring_1_no_zero_divisors_integer;
     semidom_idom = semidom_integer;
     comm_semiring_1_cancel_crossproduct_idom =
       comm_semiring_1_cancel_crossproduct_integer}
    : Z.t idom);;

let divide_integer = ({divide = divide_integera} : Z.t divide);;

let semidom_divide_integer =
  ({divide_semidom_divide = divide_integer;
     semidom_semidom_divide = semidom_integer;
     semiring_no_zero_divisors_cancel_semidom_divide =
       semiring_no_zero_divisors_cancel_integer}
    : Z.t semidom_divide);;

let idom_divide_integer =
  ({idom_idom_divide = idom_integer;
     semidom_divide_idom_divide = semidom_divide_integer}
    : Z.t idom_divide);;

let times_real_alg = ({times = times_real_alga} : real_alg times);;

let dvd_real_alg = ({times_dvd = times_real_alg} : real_alg dvd);;

let abs_real_alg = ({abs = abs_real_alga} : real_alg abs);;

let one_real_alg = ({one = one_real_alga} : real_alg one);;

let sgn_real_alg = ({sgn = sgn_real_alga} : real_alg sgn);;

let plus_real_alg = ({plus = plus_real_alga} : real_alg plus);;

let semigroup_add_real_alg =
  ({plus_semigroup_add = plus_real_alg} : real_alg semigroup_add);;

let cancel_semigroup_add_real_alg =
  ({semigroup_add_cancel_semigroup_add = semigroup_add_real_alg} :
    real_alg cancel_semigroup_add);;

let ab_semigroup_add_real_alg =
  ({semigroup_add_ab_semigroup_add = semigroup_add_real_alg} :
    real_alg ab_semigroup_add);;

let minus_real_alg = ({minus = minus_real_alga} : real_alg minus);;

let cancel_ab_semigroup_add_real_alg =
  ({ab_semigroup_add_cancel_ab_semigroup_add = ab_semigroup_add_real_alg;
     cancel_semigroup_add_cancel_ab_semigroup_add =
       cancel_semigroup_add_real_alg;
     minus_cancel_ab_semigroup_add = minus_real_alg}
    : real_alg cancel_ab_semigroup_add);;

let zero_real_alg = ({zero = zero_real_alga} : real_alg zero);;

let monoid_add_real_alg =
  ({semigroup_add_monoid_add = semigroup_add_real_alg;
     zero_monoid_add = zero_real_alg}
    : real_alg monoid_add);;

let comm_monoid_add_real_alg =
  ({ab_semigroup_add_comm_monoid_add = ab_semigroup_add_real_alg;
     monoid_add_comm_monoid_add = monoid_add_real_alg}
    : real_alg comm_monoid_add);;

let cancel_comm_monoid_add_real_alg =
  ({cancel_ab_semigroup_add_cancel_comm_monoid_add =
      cancel_ab_semigroup_add_real_alg;
     comm_monoid_add_cancel_comm_monoid_add = comm_monoid_add_real_alg}
    : real_alg cancel_comm_monoid_add);;

let mult_zero_real_alg =
  ({times_mult_zero = times_real_alg; zero_mult_zero = zero_real_alg} :
    real_alg mult_zero);;

let semigroup_mult_real_alg =
  ({times_semigroup_mult = times_real_alg} : real_alg semigroup_mult);;

let semiring_real_alg =
  ({ab_semigroup_add_semiring = ab_semigroup_add_real_alg;
     semigroup_mult_semiring = semigroup_mult_real_alg}
    : real_alg semiring);;

let semiring_0_real_alg =
  ({comm_monoid_add_semiring_0 = comm_monoid_add_real_alg;
     mult_zero_semiring_0 = mult_zero_real_alg;
     semiring_semiring_0 = semiring_real_alg}
    : real_alg semiring_0);;

let semiring_0_cancel_real_alg =
  ({cancel_comm_monoid_add_semiring_0_cancel = cancel_comm_monoid_add_real_alg;
     semiring_0_semiring_0_cancel = semiring_0_real_alg}
    : real_alg semiring_0_cancel);;

let ab_semigroup_mult_real_alg =
  ({semigroup_mult_ab_semigroup_mult = semigroup_mult_real_alg} :
    real_alg ab_semigroup_mult);;

let comm_semiring_real_alg =
  ({ab_semigroup_mult_comm_semiring = ab_semigroup_mult_real_alg;
     semiring_comm_semiring = semiring_real_alg}
    : real_alg comm_semiring);;

let comm_semiring_0_real_alg =
  ({comm_semiring_comm_semiring_0 = comm_semiring_real_alg;
     semiring_0_comm_semiring_0 = semiring_0_real_alg}
    : real_alg comm_semiring_0);;

let comm_semiring_0_cancel_real_alg =
  ({comm_semiring_0_comm_semiring_0_cancel = comm_semiring_0_real_alg;
     semiring_0_cancel_comm_semiring_0_cancel = semiring_0_cancel_real_alg}
    : real_alg comm_semiring_0_cancel);;

let power_real_alg =
  ({one_power = one_real_alg; times_power = times_real_alg} : real_alg power);;

let monoid_mult_real_alg =
  ({semigroup_mult_monoid_mult = semigroup_mult_real_alg;
     power_monoid_mult = power_real_alg}
    : real_alg monoid_mult);;

let numeral_real_alg =
  ({one_numeral = one_real_alg; semigroup_add_numeral = semigroup_add_real_alg}
    : real_alg numeral);;

let semiring_numeral_real_alg =
  ({monoid_mult_semiring_numeral = monoid_mult_real_alg;
     numeral_semiring_numeral = numeral_real_alg;
     semiring_semiring_numeral = semiring_real_alg}
    : real_alg semiring_numeral);;

let zero_neq_one_real_alg =
  ({one_zero_neq_one = one_real_alg; zero_zero_neq_one = zero_real_alg} :
    real_alg zero_neq_one);;

let semiring_1_real_alg =
  ({semiring_numeral_semiring_1 = semiring_numeral_real_alg;
     semiring_0_semiring_1 = semiring_0_real_alg;
     zero_neq_one_semiring_1 = zero_neq_one_real_alg}
    : real_alg semiring_1);;

let semiring_1_cancel_real_alg =
  ({semiring_0_cancel_semiring_1_cancel = semiring_0_cancel_real_alg;
     semiring_1_semiring_1_cancel = semiring_1_real_alg}
    : real_alg semiring_1_cancel);;

let comm_monoid_mult_real_alg =
  ({ab_semigroup_mult_comm_monoid_mult = ab_semigroup_mult_real_alg;
     monoid_mult_comm_monoid_mult = monoid_mult_real_alg;
     dvd_comm_monoid_mult = dvd_real_alg}
    : real_alg comm_monoid_mult);;

let comm_semiring_1_real_alg =
  ({comm_monoid_mult_comm_semiring_1 = comm_monoid_mult_real_alg;
     comm_semiring_0_comm_semiring_1 = comm_semiring_0_real_alg;
     semiring_1_comm_semiring_1 = semiring_1_real_alg}
    : real_alg comm_semiring_1);;

let comm_semiring_1_cancel_real_alg =
  ({comm_semiring_0_cancel_comm_semiring_1_cancel =
      comm_semiring_0_cancel_real_alg;
     comm_semiring_1_comm_semiring_1_cancel = comm_semiring_1_real_alg;
     semiring_1_cancel_comm_semiring_1_cancel = semiring_1_cancel_real_alg}
    : real_alg comm_semiring_1_cancel);;

let comm_semiring_1_cancel_crossproduct_real_alg =
  ({comm_semiring_1_cancel_comm_semiring_1_cancel_crossproduct =
      comm_semiring_1_cancel_real_alg}
    : real_alg comm_semiring_1_cancel_crossproduct);;

let semiring_no_zero_divisors_real_alg =
  ({semiring_0_semiring_no_zero_divisors = semiring_0_real_alg} :
    real_alg semiring_no_zero_divisors);;

let semiring_1_no_zero_divisors_real_alg =
  ({semiring_1_semiring_1_no_zero_divisors = semiring_1_real_alg;
     semiring_no_zero_divisors_semiring_1_no_zero_divisors =
       semiring_no_zero_divisors_real_alg}
    : real_alg semiring_1_no_zero_divisors);;

let semiring_no_zero_divisors_cancel_real_alg =
  ({semiring_no_zero_divisors_semiring_no_zero_divisors_cancel =
      semiring_no_zero_divisors_real_alg}
    : real_alg semiring_no_zero_divisors_cancel);;

let uminus_real_alg = ({uminus = uminus_real_alga} : real_alg uminus);;

let group_add_real_alg =
  ({cancel_semigroup_add_group_add = cancel_semigroup_add_real_alg;
     minus_group_add = minus_real_alg;
     monoid_add_group_add = monoid_add_real_alg;
     uminus_group_add = uminus_real_alg}
    : real_alg group_add);;

let ab_group_add_real_alg =
  ({cancel_comm_monoid_add_ab_group_add = cancel_comm_monoid_add_real_alg;
     group_add_ab_group_add = group_add_real_alg}
    : real_alg ab_group_add);;

let ring_real_alg =
  ({ab_group_add_ring = ab_group_add_real_alg;
     semiring_0_cancel_ring = semiring_0_cancel_real_alg}
    : real_alg ring);;

let ring_no_zero_divisors_real_alg =
  ({ring_ring_no_zero_divisors = ring_real_alg;
     semiring_no_zero_divisors_cancel_ring_no_zero_divisors =
       semiring_no_zero_divisors_cancel_real_alg}
    : real_alg ring_no_zero_divisors);;

let neg_numeral_real_alg =
  ({group_add_neg_numeral = group_add_real_alg;
     numeral_neg_numeral = numeral_real_alg}
    : real_alg neg_numeral);;

let ring_1_real_alg =
  ({neg_numeral_ring_1 = neg_numeral_real_alg; ring_ring_1 = ring_real_alg;
     semiring_1_cancel_ring_1 = semiring_1_cancel_real_alg}
    : real_alg ring_1);;

let ring_1_no_zero_divisors_real_alg =
  ({ring_1_ring_1_no_zero_divisors = ring_1_real_alg;
     ring_no_zero_divisors_ring_1_no_zero_divisors =
       ring_no_zero_divisors_real_alg;
     semiring_1_no_zero_divisors_ring_1_no_zero_divisors =
       semiring_1_no_zero_divisors_real_alg}
    : real_alg ring_1_no_zero_divisors);;

let comm_ring_real_alg =
  ({comm_semiring_0_cancel_comm_ring = comm_semiring_0_cancel_real_alg;
     ring_comm_ring = ring_real_alg}
    : real_alg comm_ring);;

let comm_ring_1_real_alg =
  ({comm_ring_comm_ring_1 = comm_ring_real_alg;
     comm_semiring_1_cancel_comm_ring_1 = comm_semiring_1_cancel_real_alg;
     ring_1_comm_ring_1 = ring_1_real_alg}
    : real_alg comm_ring_1);;

let semidom_real_alg =
  ({comm_semiring_1_cancel_semidom = comm_semiring_1_cancel_real_alg;
     semiring_1_no_zero_divisors_semidom = semiring_1_no_zero_divisors_real_alg}
    : real_alg semidom);;

let idom_real_alg =
  ({comm_ring_1_idom = comm_ring_1_real_alg;
     ring_1_no_zero_divisors_idom = ring_1_no_zero_divisors_real_alg;
     semidom_idom = semidom_real_alg;
     comm_semiring_1_cancel_crossproduct_idom =
       comm_semiring_1_cancel_crossproduct_real_alg}
    : real_alg idom);;

let ufd_real_alg = ({idom_ufd = idom_real_alg} : real_alg ufd);;

let divide_real_alg = ({divide = divide_real_alga} : real_alg divide);;

let inverse_real_alg =
  ({divide_inverse = divide_real_alg; inverse = inverse_real_alga} :
    real_alg inverse);;

let division_ring_real_alg =
  ({inverse_division_ring = inverse_real_alg;
     ring_1_no_zero_divisors_division_ring = ring_1_no_zero_divisors_real_alg}
    : real_alg division_ring);;

let semidom_divide_real_alg =
  ({divide_semidom_divide = divide_real_alg;
     semidom_semidom_divide = semidom_real_alg;
     semiring_no_zero_divisors_cancel_semidom_divide =
       semiring_no_zero_divisors_cancel_real_alg}
    : real_alg semidom_divide);;

let idom_divide_real_alg =
  ({idom_idom_divide = idom_real_alg;
     semidom_divide_idom_divide = semidom_divide_real_alg}
    : real_alg idom_divide);;

let field_real_alg =
  ({division_ring_field = division_ring_real_alg;
     idom_divide_field = idom_divide_real_alg; ufd_field = ufd_real_alg}
    : real_alg field);;

let ord_real_alg =
  ({less_eq = less_eq_real_alg; less = less_real_alg} : real_alg ord);;

let abs_if_real_alg =
  ({abs_abs_if = abs_real_alg; minus_abs_if = minus_real_alg;
     uminus_abs_if = uminus_real_alg; zero_abs_if = zero_real_alg;
     ord_abs_if = ord_real_alg}
    : real_alg abs_if);;

let semiring_char_0_real_alg =
  ({semiring_1_semiring_char_0 = semiring_1_real_alg} :
    real_alg semiring_char_0);;

let ring_char_0_real_alg =
  ({semiring_char_0_ring_char_0 = semiring_char_0_real_alg;
     ring_1_ring_char_0 = ring_1_real_alg}
    : real_alg ring_char_0);;

let preorder_real_alg = ({ord_preorder = ord_real_alg} : real_alg preorder);;

let order_real_alg = ({preorder_order = preorder_real_alg} : real_alg order);;

let no_bot_real_alg = ({order_no_bot = order_real_alg} : real_alg no_bot);;

let no_top_real_alg = ({order_no_top = order_real_alg} : real_alg no_top);;

let linorder_real_alg =
  ({order_linorder = order_real_alg} : real_alg linorder);;

let idom_abs_sgn_real_alg =
  ({abs_idom_abs_sgn = abs_real_alg; sgn_idom_abs_sgn = sgn_real_alg;
     idom_idom_abs_sgn = idom_real_alg}
    : real_alg idom_abs_sgn);;

let ordered_ab_semigroup_add_real_alg =
  ({ab_semigroup_add_ordered_ab_semigroup_add = ab_semigroup_add_real_alg;
     order_ordered_ab_semigroup_add = order_real_alg}
    : real_alg ordered_ab_semigroup_add);;

let strict_ordered_ab_semigroup_add_real_alg =
  ({ordered_ab_semigroup_add_strict_ordered_ab_semigroup_add =
      ordered_ab_semigroup_add_real_alg}
    : real_alg strict_ordered_ab_semigroup_add);;

let ordered_cancel_ab_semigroup_add_real_alg =
  ({cancel_ab_semigroup_add_ordered_cancel_ab_semigroup_add =
      cancel_ab_semigroup_add_real_alg;
     strict_ordered_ab_semigroup_add_ordered_cancel_ab_semigroup_add =
       strict_ordered_ab_semigroup_add_real_alg}
    : real_alg ordered_cancel_ab_semigroup_add);;

let ordered_comm_monoid_add_real_alg =
  ({comm_monoid_add_ordered_comm_monoid_add = comm_monoid_add_real_alg;
     ordered_ab_semigroup_add_ordered_comm_monoid_add =
       ordered_ab_semigroup_add_real_alg}
    : real_alg ordered_comm_monoid_add);;

let ordered_semiring_real_alg =
  ({ordered_comm_monoid_add_ordered_semiring = ordered_comm_monoid_add_real_alg;
     semiring_ordered_semiring = semiring_real_alg}
    : real_alg ordered_semiring);;

let ordered_semiring_0_real_alg =
  ({ordered_semiring_ordered_semiring_0 = ordered_semiring_real_alg;
     semiring_0_ordered_semiring_0 = semiring_0_real_alg}
    : real_alg ordered_semiring_0);;

let ordered_cancel_semiring_real_alg =
  ({ordered_cancel_ab_semigroup_add_ordered_cancel_semiring =
      ordered_cancel_ab_semigroup_add_real_alg;
     ordered_semiring_0_ordered_cancel_semiring = ordered_semiring_0_real_alg;
     semiring_0_cancel_ordered_cancel_semiring = semiring_0_cancel_real_alg}
    : real_alg ordered_cancel_semiring);;

let ordered_ab_semigroup_add_imp_le_real_alg =
  ({ordered_cancel_ab_semigroup_add_ordered_ab_semigroup_add_imp_le =
      ordered_cancel_ab_semigroup_add_real_alg}
    : real_alg ordered_ab_semigroup_add_imp_le);;

let strict_ordered_comm_monoid_add_real_alg =
  ({comm_monoid_add_strict_ordered_comm_monoid_add = comm_monoid_add_real_alg;
     strict_ordered_ab_semigroup_add_strict_ordered_comm_monoid_add =
       strict_ordered_ab_semigroup_add_real_alg}
    : real_alg strict_ordered_comm_monoid_add);;

let ordered_cancel_comm_monoid_add_real_alg =
  ({ordered_cancel_ab_semigroup_add_ordered_cancel_comm_monoid_add =
      ordered_cancel_ab_semigroup_add_real_alg;
     ordered_comm_monoid_add_ordered_cancel_comm_monoid_add =
       ordered_comm_monoid_add_real_alg;
     strict_ordered_comm_monoid_add_ordered_cancel_comm_monoid_add =
       strict_ordered_comm_monoid_add_real_alg}
    : real_alg ordered_cancel_comm_monoid_add);;

let ordered_ab_semigroup_monoid_add_imp_le_real_alg =
  ({cancel_comm_monoid_add_ordered_ab_semigroup_monoid_add_imp_le =
      cancel_comm_monoid_add_real_alg;
     ordered_ab_semigroup_add_imp_le_ordered_ab_semigroup_monoid_add_imp_le =
       ordered_ab_semigroup_add_imp_le_real_alg;
     ordered_cancel_comm_monoid_add_ordered_ab_semigroup_monoid_add_imp_le =
       ordered_cancel_comm_monoid_add_real_alg}
    : real_alg ordered_ab_semigroup_monoid_add_imp_le);;

let ordered_ab_group_add_real_alg =
  ({ab_group_add_ordered_ab_group_add = ab_group_add_real_alg;
     ordered_ab_semigroup_monoid_add_imp_le_ordered_ab_group_add =
       ordered_ab_semigroup_monoid_add_imp_le_real_alg}
    : real_alg ordered_ab_group_add);;

let ordered_ring_real_alg =
  ({ordered_ab_group_add_ordered_ring = ordered_ab_group_add_real_alg;
     ordered_cancel_semiring_ordered_ring = ordered_cancel_semiring_real_alg;
     ring_ordered_ring = ring_real_alg}
    : real_alg ordered_ring);;

let field_char_0_real_alg =
  ({field_field_char_0 = field_real_alg;
     ring_char_0_field_char_0 = ring_char_0_real_alg}
    : real_alg field_char_0);;

let zero_less_one_real_alg =
  ({order_zero_less_one = order_real_alg;
     zero_neq_one_zero_less_one = zero_neq_one_real_alg}
    : real_alg zero_less_one);;

let field_abs_sgn_real_alg =
  ({field_field_abs_sgn = field_real_alg;
     idom_abs_sgn_field_abs_sgn = idom_abs_sgn_real_alg}
    : real_alg field_abs_sgn);;

let dense_order_real_alg =
  ({order_dense_order = order_real_alg} : real_alg dense_order);;

let linordered_ab_semigroup_add_real_alg =
  ({ordered_ab_semigroup_add_linordered_ab_semigroup_add =
      ordered_ab_semigroup_add_real_alg;
     linorder_linordered_ab_semigroup_add = linorder_real_alg}
    : real_alg linordered_ab_semigroup_add);;

let linordered_cancel_ab_semigroup_add_real_alg =
  ({linordered_ab_semigroup_add_linordered_cancel_ab_semigroup_add =
      linordered_ab_semigroup_add_real_alg;
     ordered_ab_semigroup_add_imp_le_linordered_cancel_ab_semigroup_add =
       ordered_ab_semigroup_add_imp_le_real_alg}
    : real_alg linordered_cancel_ab_semigroup_add);;

let linordered_semiring_real_alg =
  ({linordered_cancel_ab_semigroup_add_linordered_semiring =
      linordered_cancel_ab_semigroup_add_real_alg;
     ordered_ab_semigroup_monoid_add_imp_le_linordered_semiring =
       ordered_ab_semigroup_monoid_add_imp_le_real_alg;
     ordered_cancel_semiring_linordered_semiring =
       ordered_cancel_semiring_real_alg}
    : real_alg linordered_semiring);;

let linordered_semiring_strict_real_alg =
  ({linordered_semiring_linordered_semiring_strict =
      linordered_semiring_real_alg}
    : real_alg linordered_semiring_strict);;

let linordered_semiring_1_real_alg =
  ({linordered_semiring_linordered_semiring_1 = linordered_semiring_real_alg;
     semiring_1_linordered_semiring_1 = semiring_1_real_alg;
     zero_less_one_linordered_semiring_1 = zero_less_one_real_alg}
    : real_alg linordered_semiring_1);;

let linordered_semiring_1_strict_real_alg =
  ({linordered_semiring_1_linordered_semiring_1_strict =
      linordered_semiring_1_real_alg;
     linordered_semiring_strict_linordered_semiring_1_strict =
       linordered_semiring_strict_real_alg}
    : real_alg linordered_semiring_1_strict);;

let ordered_ab_group_add_abs_real_alg =
  ({abs_ordered_ab_group_add_abs = abs_real_alg;
     ordered_ab_group_add_ordered_ab_group_add_abs =
       ordered_ab_group_add_real_alg}
    : real_alg ordered_ab_group_add_abs);;

let linordered_ab_group_add_real_alg =
  ({linordered_cancel_ab_semigroup_add_linordered_ab_group_add =
      linordered_cancel_ab_semigroup_add_real_alg;
     ordered_ab_group_add_linordered_ab_group_add =
       ordered_ab_group_add_real_alg}
    : real_alg linordered_ab_group_add);;

let linordered_ring_real_alg =
  ({linordered_ab_group_add_linordered_ring = linordered_ab_group_add_real_alg;
     ordered_ab_group_add_abs_linordered_ring =
       ordered_ab_group_add_abs_real_alg;
     abs_if_linordered_ring = abs_if_real_alg;
     linordered_semiring_linordered_ring = linordered_semiring_real_alg;
     ordered_ring_linordered_ring = ordered_ring_real_alg}
    : real_alg linordered_ring);;

let linordered_ring_strict_real_alg =
  ({linordered_ring_linordered_ring_strict = linordered_ring_real_alg;
     linordered_semiring_strict_linordered_ring_strict =
       linordered_semiring_strict_real_alg;
     ring_no_zero_divisors_linordered_ring_strict =
       ring_no_zero_divisors_real_alg}
    : real_alg linordered_ring_strict);;

let ordered_comm_semiring_real_alg =
  ({comm_semiring_0_ordered_comm_semiring = comm_semiring_0_real_alg;
     ordered_semiring_ordered_comm_semiring = ordered_semiring_real_alg}
    : real_alg ordered_comm_semiring);;

let ordered_cancel_comm_semiring_real_alg =
  ({comm_semiring_0_cancel_ordered_cancel_comm_semiring =
      comm_semiring_0_cancel_real_alg;
     ordered_cancel_semiring_ordered_cancel_comm_semiring =
       ordered_cancel_semiring_real_alg;
     ordered_comm_semiring_ordered_cancel_comm_semiring =
       ordered_comm_semiring_real_alg}
    : real_alg ordered_cancel_comm_semiring);;

let linordered_comm_semiring_strict_real_alg =
  ({linordered_semiring_strict_linordered_comm_semiring_strict =
      linordered_semiring_strict_real_alg;
     ordered_cancel_comm_semiring_linordered_comm_semiring_strict =
       ordered_cancel_comm_semiring_real_alg}
    : real_alg linordered_comm_semiring_strict);;

let linordered_nonzero_semiring_real_alg =
  ({semiring_char_0_linordered_nonzero_semiring = semiring_char_0_real_alg;
     linorder_linordered_nonzero_semiring = linorder_real_alg;
     comm_semiring_1_linordered_nonzero_semiring = comm_semiring_1_real_alg;
     ordered_comm_semiring_linordered_nonzero_semiring =
       ordered_comm_semiring_real_alg;
     zero_less_one_linordered_nonzero_semiring = zero_less_one_real_alg}
    : real_alg linordered_nonzero_semiring);;

let linordered_semidom_real_alg =
  ({linordered_comm_semiring_strict_linordered_semidom =
      linordered_comm_semiring_strict_real_alg;
     linordered_nonzero_semiring_linordered_semidom =
       linordered_nonzero_semiring_real_alg;
     semidom_linordered_semidom = semidom_real_alg}
    : real_alg linordered_semidom);;

let ordered_comm_ring_real_alg =
  ({comm_ring_ordered_comm_ring = comm_ring_real_alg;
     ordered_cancel_comm_semiring_ordered_comm_ring =
       ordered_cancel_comm_semiring_real_alg;
     ordered_ring_ordered_comm_ring = ordered_ring_real_alg}
    : real_alg ordered_comm_ring);;

let ordered_ring_abs_real_alg =
  ({ordered_ab_group_add_abs_ordered_ring_abs =
      ordered_ab_group_add_abs_real_alg;
     ordered_ring_ordered_ring_abs = ordered_ring_real_alg}
    : real_alg ordered_ring_abs);;

let linordered_idom_real_alg =
  ({ring_char_0_linordered_idom = ring_char_0_real_alg;
     idom_abs_sgn_linordered_idom = idom_abs_sgn_real_alg;
     linordered_ring_strict_linordered_idom = linordered_ring_strict_real_alg;
     linordered_semidom_linordered_idom = linordered_semidom_real_alg;
     linordered_semiring_1_strict_linordered_idom =
       linordered_semiring_1_strict_real_alg;
     ordered_comm_ring_linordered_idom = ordered_comm_ring_real_alg;
     ordered_ring_abs_linordered_idom = ordered_ring_abs_real_alg}
    : real_alg linordered_idom);;

let non_strict_order_real_alg =
  ({ord_non_strict_order = ord_real_alg} : real_alg non_strict_order);;

let ordered_ab_semigroup_real_alg =
  ({ab_semigroup_add_ordered_ab_semigroup = ab_semigroup_add_real_alg;
     monoid_add_ordered_ab_semigroup = monoid_add_real_alg;
     non_strict_order_ordered_ab_semigroup = non_strict_order_real_alg}
    : real_alg ordered_ab_semigroup);;

let ordered_semiring_0_real_alga =
  ({semiring_0_ordered_semiring_0a = semiring_0_real_alg;
     ordered_ab_semigroup_ordered_semiring_0 = ordered_ab_semigroup_real_alg}
    : real_alg ordered_semiring_0a);;

let ordered_semiring_1_real_alg =
  ({semiring_1_ordered_semiring_1 = semiring_1_real_alg;
     ordered_semiring_0_ordered_semiring_1 = ordered_semiring_0_real_alga}
    : real_alg ordered_semiring_1);;

let poly_carrier_real_alg =
  ({comm_semiring_1_poly_carrier = comm_semiring_1_real_alg;
     ordered_semiring_1_poly_carrier = ordered_semiring_1_real_alg}
    : real_alg poly_carrier);;

let dense_linorder_real_alg =
  ({dense_order_dense_linorder = dense_order_real_alg;
     linorder_dense_linorder = linorder_real_alg}
    : real_alg dense_linorder);;

let unbounded_dense_linorder_real_alg =
  ({dense_linorder_unbounded_dense_linorder = dense_linorder_real_alg;
     no_bot_unbounded_dense_linorder = no_bot_real_alg;
     no_top_unbounded_dense_linorder = no_top_real_alg}
    : real_alg unbounded_dense_linorder);;

let linordered_field_real_alg =
  ({field_abs_sgn_linordered_field = field_abs_sgn_real_alg;
     field_char_0_linordered_field = field_char_0_real_alg;
     unbounded_dense_linorder_linordered_field =
       unbounded_dense_linorder_real_alg;
     linordered_idom_linordered_field = linordered_idom_real_alg}
    : real_alg linordered_field);;

let archimedean_field_real_alg =
  ({linordered_field_archimedean_field = linordered_field_real_alg} :
    real_alg archimedean_field);;

let large_ordered_semiring_1_real_alg =
  ({poly_carrier_large_ordered_semiring_1 = poly_carrier_real_alg} :
    real_alg large_ordered_semiring_1);;

let floor_ceiling_real_alg =
  ({archimedean_field_floor_ceiling = archimedean_field_real_alg;
     large_ordered_semiring_1_floor_ceiling = large_ordered_semiring_1_real_alg;
     floor = floor_real_alg}
    : real_alg floor_ceiling);;

type 'a interval = Interval of 'a * 'a;;

type complex_interval = Complex_Interval of real interval * real interval;;

let rec plus_interval _A
  (Interval (lx, ux)) (Interval (ly, uy)) =
    Interval (plus _A lx ly, plus _A ux uy);;

let rec plus_complex_intervala
  (Complex_Interval (rx, ix)) (Complex_Interval (ry, iy)) =
    Complex_Interval
      (plus_interval plus_real rx ry, plus_interval plus_real ix iy);;

let plus_complex_interval =
  ({plus = plus_complex_intervala} : complex_interval plus);;

let rec zero_interval _A = Interval (zero _A, zero _A);;

let zero_complex_intervala : complex_interval
  = Complex_Interval (zero_interval zero_real, zero_interval zero_real);;

let zero_complex_interval =
  ({zero = zero_complex_intervala} : complex_interval zero);;

let rec times_interval (_A1, _A2)
  (Interval (lx, ux)) (Interval (ly, uy)) =
    (let x1 = times _A1 lx ly in
     let x2 = times _A1 lx uy in
     let x3 = times _A1 ux ly in
     let x4 = times _A1 ux uy in
      Interval
        (min _A2 x1 (min _A2 x2 (min _A2 x3 x4)),
          max _A2 x1 (max _A2 x2 (max _A2 x3 x4))));;

let rec minus_interval _A
  (Interval (lx, ux)) (Interval (ly, uy)) =
    Interval (minus _A lx uy, minus _A ux ly);;

let rec times_complex_intervala
  (Complex_Interval (rx, ix)) (Complex_Interval (ry, iy)) =
    Complex_Interval
      (minus_interval minus_real (times_interval (times_real, ord_real) rx ry)
         (times_interval (times_real, ord_real) ix iy),
        plus_interval plus_real (times_interval (times_real, ord_real) rx iy)
          (times_interval (times_real, ord_real) ix ry));;

let times_complex_interval =
  ({times = times_complex_intervala} : complex_interval times);;

type ('a, 'b) sum = Inl of 'a | Inr of 'b;;

type 'a genuine_roots_aux =
  Abs_genuine_roots_aux of ('a poly * ('a list * (nat * (nat -> 'a -> bool))));;

type 'a rf_poly = Abs_rf_poly of 'a poly;;

type 'a rf_polys = Abs_rf_polys of ('a * ('a poly * nat) list);;

type real_alg_show_info = Rat_Info of rat | Sqrt_Info of rat * rat |
  Real_Alg_Info of int poly * nat;;

type real_alg_3_list_x_x_real_alg_2_list =
  Abs_real_alg_3_list_x_x_real_alg_2_list of real_alg_2 list;;

type 'a nat_x_idom_x_x_a_rf_poly_prod_x_x_nat_x_idom_x_x_a_poly_prod =
  Abs_nat_x_idom_x_x_a_rf_poly_prod_x_x_nat_x_idom_x_x_a_poly_prod of
    ('a poly * nat);;

type 'a nat_x_idom_x_x_a_rf_poly_prod_list_x_x_nat_x_idom_x_x_a_poly_prod_list =
  Abs_nat_x_idom_x_x_a_rf_poly_prod_list_x_x_nat_x_idom_x_x_a_poly_prod_list of
    ('a poly * nat) list;;

type 'a nat_x_idom_x_x_a_rf_poly_prod_list_x_idom_x_x_a_prod_x_x_nat_x_idom_x_x_a_poly_prod_list_x_idom_x_x_a_prod
  = Abs_nat_x_idom_x_x_a_rf_poly_prod_list_x_idom_x_x_a_prod_x_x_nat_x_idom_x_x_a_poly_prod_list_x_idom_x_x_a_prod
      of ('a * ('a poly * nat) list);;

let rec col a j = vec (dim_row a) (fun i -> index_mat a (i, j));;

let rec row a i = vec (dim_col a) (fun j -> index_mat a (i, j));;

let rec filter (_A1, _A2) p a = inf_seta (_A1, _A2) a (Collect_set p);;

let rec cols a = map (col a) (upt zero_nata (dim_col a));;

let rec rows a = map (row a) (upt zero_nata (dim_row a));;

let rec list_ex p x1 = match p, x1 with p, [] -> false
                  | p, x :: xs -> p x || list_ex p xs;;

let rec remdups _A
  = function [] -> []
    | x :: xs ->
        (if membera _A xs x then remdups _A xs else x :: remdups _A xs);;

let rec poly_rat
  x = (let (n, d) = quotient_of x in
        pCons (zero_int, equal_int) (uminus_inta n)
          (pCons (zero_int, equal_int) d (zero_polya zero_int)));;

let rec of_rat_1 x = (poly_rat x, (x, x));;

let rec real_alg_1 = function Rational r -> of_rat_1 r
                     | Irrational (n, rai) -> rai;;

let rec root_rat_floor
  p x = (let (a, b) = quotient_of x in
          divide_inta
            (root_int_floor p
              (times_inta a
                (binary_power monoid_mult_int b (minus_nata p one_nata))))
            b);;

let rec root_rat_ceiling p x = uminus_inta (root_rat_floor p (uminus_rata x));;

let rec initial_upper_bound n r = of_int (root_rat_ceiling n r);;

let rec initial_lower_bound
  n l = (if less_eq_rat l one_rata then l else of_int (root_rat_floor n l));;

let rec tighten_bound_root
  n cmpx (l, r) =
    (let m = divide_rata (plus_rata l r) (of_int (Int_of_integer (Z.of_int 2)))
       in
     let ma = binary_power monoid_mult_rat m n in
      (match cmpx ma with Eq -> (m, m) | Lt -> (m, r) | Gt -> (l, m)));;

let rec poly_nth_root (_A1, _A2)
  n p = pcompose
          (_A1, _A2.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_semiring_0_comm_semiring_1,
            _A2.semidom_idom.semiring_1_no_zero_divisors_semidom.semiring_no_zero_divisors_semiring_1_no_zero_divisors)
          p (monoma
              (_A2.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
                _A1)
              (one _A2.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.numeral_neg_numeral.one_numeral)
              n);;

let rec compare_1_rat
  rai = (let p = poly_real_alg_1 rai in
          (if equal_nata (degreea zero_int p) one_nata
            then (let x =
                    fract (uminus_inta
                            (match coeffs zero_int p with [] -> zero_inta
                              | x :: _ -> x))
                      (coeffa zero_int p one_nata)
                    in
                   (fun y -> compare_rat y x))
            else (fun y -> compare_rat_1 y rai)));;

let rec root_pos_1
  n (p, (l, r)) =
    select_correct_factor_int_poly
      (tighten_bound_root n (compare_1_rat (p, (l, r)))) (fun x -> x)
      (initial_lower_bound n l, initial_upper_bound n r)
      (poly_nth_root (equal_int, idom_int) n p);;

let rec root_1
  n (p, (l, r)) =
    (if equal_nata n zero_nata || equal_rata r zero_rata then Rational zero_rata
      else (if less_rat zero_rata r then root_pos_1 n (p, (l, r))
             else uminus_2 (root_pos_1 n (uminus_1 (p, (l, r))))));;

let rec root_2 n x = root_1 n (real_alg_1 x);;

let rec root_3 xb xc = Real_Alg_Invariant (root_2 xb (rep_real_alg_3 xc));;

let rec root_real_alg
  xa (Real_Alg_Quotient x) = Real_Alg_Quotient (root_3 xa x);;

let rec root n (Real_of x) = Real_of (root_real_alg n x);;

let rec sqrt x = root (nat_of_integer (Z.of_int 2)) x;;

let rec ratreal x = comp (fun a -> Real_of a) of_rat_real_alg x;;

let rec norm_complex
  z = sqrt (plus_reala
             (binary_power monoid_mult_real (re z)
               (nat_of_integer (Z.of_int 2)))
             (binary_power monoid_mult_real (im z)
               (nat_of_integer (Z.of_int 2))));;

let rec csqrt
  z = Complex
        (sqrt (divide_reala (plus_reala (norm_complex z) (re z))
                (ratreal (of_int (Int_of_integer (Z.of_int 2))))),
          times_reala
            (if equal_reala (im z) zero_reala then one_reala
              else sgn_reala (im z))
            (sqrt (divide_reala (minus_reala (norm_complex z) (re z))
                    (ratreal (of_int (Int_of_integer (Z.of_int 2)))))));;

let rec dim_vec_impl xa = fst (rep_vec_impl xa);;

let rec dim_vec (Vec_impl v) = dim_vec_impl v;;

let rec map_mat
  f a = mat (dim_row a) (dim_col a) (fun ij -> f (index_mat a ij));;

let rec map_vec f v = vec (dim_vec v) (fun i -> f (vec_index v i));;

let rec one_mat (_A1, _A2)
  n = mat n n (fun (i, j) -> (if equal_nata i j then one _A1 else zero _A2));;

let rec scalar_prod _A
  v w = sum_list _A.comm_monoid_add_semiring_0.monoid_add_comm_monoid_add
          (map (fun i ->
                 times _A.mult_zero_semiring_0.times_mult_zero (vec_index v i)
                   (vec_index w i))
            (upt zero_nata (dim_vec w)));;

let rec times_mat _A
  a b = mat (dim_row a) (dim_col b)
          (fun (i, j) -> scalar_prod _A (row a i) (col b j));;

let rec pow_mat _A
  a k = (if equal_nata k zero_nata
          then one_mat
                 (_A.semiring_numeral_semiring_1.numeral_semiring_numeral.one_numeral,
                   _A.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
                 (dim_row a)
          else times_mat _A.semiring_0_semiring_1
                 (pow_mat _A a (minus_nata k one_nata)) a);;

let rec of_bool _A = function true -> one _A.one_zero_neq_one
                     | false -> zero _A.zero_zero_neq_one;;

let rec integer_of_char
  (Chara (b0, b1, b2, b3, b4, b5, b6, b7)) =
    Z.add (Z.mul
            (Z.add
              (Z.mul
                (Z.add
                  (Z.mul
                    (Z.add
                      (Z.mul
                        (Z.add
                          (Z.mul
                            (Z.add
                              (Z.mul
                                (Z.add
                                  (Z.mul (of_bool zero_neq_one_integer b7)
                                    (Z.of_int 2))
                                  (of_bool zero_neq_one_integer b6))
                                (Z.of_int 2))
                              (of_bool zero_neq_one_integer b5))
                            (Z.of_int 2))
                          (of_bool zero_neq_one_integer b4))
                        (Z.of_int 2))
                      (of_bool zero_neq_one_integer b3))
                    (Z.of_int 2))
                  (of_bool zero_neq_one_integer b2))
                (Z.of_int 2))
              (of_bool zero_neq_one_integer b1))
            (Z.of_int 2))
      (of_bool zero_neq_one_integer b0);;

let rec implode cs = Str_Literal.literal_of_asciis (map integer_of_char cs);;

let rec trivial_mute_fun _A
  x y = (x, (y, one _A.ring_1_comm_ring_1.neg_numeral_ring_1.numeral_neg_numeral.one_numeral));;

let rec fst_sel_fun x = fst (hda x);;

let rec prod_list _A
  xs = foldr (times _A.power_monoid_mult.times_power) xs
         (one _A.power_monoid_mult.one_power);;

let rec find_non0 (_A1, _A2)
  sel_fun l a =
    (let is = upt (suc l) (dim_row a) in
     let ais =
       filtera
         (fun (_, ail) ->
           not (eq _A1 ail
                 (zero _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)))
         (map (fun i -> (i, index_mat a (i, l))) is)
       in
      (match ais with [] -> None | _ :: _ -> Some (sel_fun ais)));;

let rec mat_addrow_gen_impl
  xd xe xf xh xi xj =
    Abs_mat_impl
      (let (nr, (nc, a)) = rep_mat_impl xj in
        (if less_nat xi nr
          then (let ak = sub a xh in
                let al = sub a xi in
                let aka =
                  of_fun (fun i -> xd (xe xf (sub al i)) (sub ak i))
                    (min ord_nat (length ak) (length al))
                  in
                let aa =
                  of_fun (fun i -> (if equal_nata i xh then aka else sub a i))
                    (length a)
                  in
                 (nr, (nc, aa)))
          else (nr, (nc, a))));;

let rec mat_addrow_gen
  ad mul aa k l (Mat_impl a) =
    (if less_nat l (dim_row_impl a)
      then Mat_impl (mat_addrow_gen_impl ad mul aa k l a)
      else failwith "index out of bounds in mat_addrow"
             (fun _ -> mat_addrow_gen ad mul aa k l (Mat_impl a)));;

let rec mute (_A1, _A2)
  mf a_ll k l (r, a) =
    (let p = index_mat a (k, l) in
      (if eq _A1 p
            (zero _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
        then (r, a)
        else (let (q, (pa, _)) = mf a_ll p in
               (times _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd
                  r q,
                 mat_addrow_gen
                   (plus _A2.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.numeral_neg_numeral.semigroup_add_numeral.plus_semigroup_add)
                   (times
                     _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd)
                   (uminus
                     _A2.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.group_add_neg_numeral.uminus_group_add
                     pa)
                   k l (mat_multrow_gen
                         (times
                           _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd)
                         k q a)))));;

let rec sub1 (_A1, _A2)
  mf q k l rA =
    (if equal_nata k zero_nata then rA
      else mute (_A1, _A2) mf q (plus_nata l (suc (minus_nata k one_nata))) l
             (sub1 (_A1, _A2) mf q (minus_nata k one_nata) l rA));;

let rec sub2 (_A1, _A2)
  sel_fun mf d l (r, a) =
    (match find_non0 (_A1, _A2) sel_fun l a with None -> (r, a)
      | Some m ->
        (let aa = mat_swaprows m l a in
          sub1 (_A1, _A2) mf (index_mat aa (l, l)) (minus_nata d (suc l)) l
            (uminus
               _A2.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.group_add_neg_numeral.uminus_group_add
               r,
              aa)));;

let rec sub3 (_A1, _A2)
  sel_fun mf d l rA =
    (if equal_nata l zero_nata then rA
      else sub2 (_A1, _A2) sel_fun mf d (minus_nata l one_nata)
             (sub3 (_A1, _A2) sel_fun mf d (minus_nata l one_nata) rA));;

let rec triangulize (_A1, _A2)
  sel_fun mf a =
    sub3 (_A1, _A2) sel_fun mf (dim_row a) (dim_row a)
      (one _A2.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.numeral_neg_numeral.one_numeral,
        a);;

let rec diag_mat
  a = map (fun i -> index_mat a (i, i)) (upt zero_nata (dim_row a));;

let rec det_code (_A1, _A2)
  sel_fun mf a =
    (if equal_nata (dim_row a) (dim_col a)
      then (let (m, aa) = triangulize (_A1, _A2) sel_fun mf a in
             divide _A2.semidom_divide_idom_divide.divide_semidom_divide
               (prod_list
                 _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_numeral_semiring_1.monoid_mult_semiring_numeral
                 (diag_mat aa))
               m)
      else zero _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero);;

let rec det (_A1, _A2)
  a = det_code (_A1, _A2) fst_sel_fun
        (trivial_mute_fun _A2.idom_idom_divide.comm_ring_1_idom) a;;

let rec unit_vec _A
  n i = vec n (fun j ->
                (if equal_nata j i then one _A.one_zero_neq_one
                  else zero _A.zero_zero_neq_one));;

let rec zero_mat _A nr nc = mat nr nc (fun _ -> zero _A);;

let rec zero_vec _A n = vec n (fun _ -> zero _A);;

let rec monom (_A1, _A2) xa x = MPoly (single (_A1, _A2) xa x);;

let rec smult_mat _A aa a = map_mat (times _A aa) a;;

let rec smult_vec _A
  a v = vec (dim_vec v) (fun i -> times _A a (vec_index v i));;

let rec map_sum f1 f2 x2 = match f1, f2, x2 with f1, f2, Inl a -> Inl (f1 a)
                  | f1, f2, Inr a -> Inr (f2 a);;

let rec arg_min_list _B
  f x1 = match f, x1 with f, [x] -> x
    | f, x :: y :: zs ->
        (let m = arg_min_list _B f (y :: zs) in
          (if less_eq _B.order_linorder.preorder_order.ord_preorder (f x) (f m)
            then x else m));;

let rec algebraic_real (Real_of x) = true;;

let rec algebraic_complex x = algebraic_real (re x) && algebraic_real (im x);;

let initial_precision : nat = nat_of_integer (Z.of_int 10);;

let rec rep_genuine_roots_aux _A (Abs_genuine_roots_aux x) = x;;

let rec genuine_roots_impl_step _A
  xb xc =
    Abs_genuine_roots_aux
      (let (p, (xs, (n, ff))) = rep_genuine_roots_aux _A xc in
        (p, (filtera (ff xb) xs, (n, ff))));;

let rec gr_numroots _A
  xa = (let (_, (_, (n, _))) = rep_genuine_roots_aux _A xa in n);;

let rec gr_list _A
  xa = (let (_, (xs, (_, _))) = rep_genuine_roots_aux _A xa in xs);;

let rec genuine_roots _A
  prec gr =
    (if equal_nata (size_list (gr_list _A gr)) (gr_numroots _A gr)
      then gr_list _A gr
      else genuine_roots _A (times_nata (nat_of_integer (Z.of_int 2)) prec)
             (genuine_roots_impl_step _A prec gr));;

let rec genuine_roots_impl _A = genuine_roots _A initial_precision;;

let poly_2i : int poly
  = pCons (zero_int, equal_int) (Int_of_integer (Z.of_int 4))
      (pCons (zero_int, equal_int) zero_inta
        (pCons (zero_int, equal_int) one_inta (zero_polya zero_int)));;

let rec root_poly_Im
  p = (let fs =
         factors_of_int_poly
           (poly_add
             (factorial_ring_gcd_int, semiring_gcd_mult_normalize_int,
               equal_int)
             p (poly_uminus (equal_int, ring_1_int) p))
         in
        remdups (equal_poly (zero_int, equal_int))
          (if list_ex
                (fun f ->
                  equal_inta
                    (match coeffs zero_int f with [] -> zero_inta | x :: _ -> x)
                    zero_inta)
                fs
            then [pCons (zero_int, equal_int) zero_inta
                    (pCons (zero_int, equal_int) one_inta
                      (zero_polya zero_int))]
            else []) @
          maps (fun f ->
                 (if not (equal_inta
                           (match coeffs zero_int f with [] -> zero_inta
                             | x :: _ -> x)
                           zero_inta)
                   then [cf_pos_poly
                           (poly_div
                             (factorial_ring_gcd_int,
                               semiring_gcd_mult_normalize_int, equal_int)
                             f poly_2i)]
                   else []))
            fs);;

let rec remdups_gen
  eq x1 = match eq, x1 with eq, [] -> []
    | eq, x :: xs ->
        (if list_ex (eq x) xs then remdups_gen eq xs
          else x :: remdups_gen eq xs);;

let rec rep_real_alg_3_list_x_x_real_alg_2_list
  (Abs_real_alg_3_list_x_x_real_alg_2_list x) = x;;

let rec sel22
  xa = Abs_real_alg_3_list_x_x_real_alg_2_list
         (match rep_real_alg_3_list_x_x_real_alg_2_list xa
           with [] ->
             rep_real_alg_3_list_x_x_real_alg_2_list (failwith "undefined")
           | _ :: x22 -> x22);;

let rec sel21
  xa = Real_Alg_Invariant
         (match rep_real_alg_3_list_x_x_real_alg_2_list xa
           with [] -> rep_real_alg_3 (failwith "undefined") | x21 :: _ -> x21);;

let rec dis1
  xa = (match rep_real_alg_3_list_x_x_real_alg_2_list xa with [] -> true
         | _ :: _ -> false);;

let rec rep_isom x = (if dis1 x then [] else sel21 x :: rep_isom (sel22 x));;

let rec max_list_non_empty _A
  = function [x] -> x
    | x :: v :: va ->
        max _A.order_linorder.preorder_order.ord_preorder x
          (max_list_non_empty _A (v :: va));;

let rec div_ceiling
  x y = (let q = divide_inta x y in
          (if equal_inta (times_inta q y) x then q else plus_inta q one_inta));;

let rec root_bound
  p = (let n = degreea zero_int p in
       let m =
         plus_inta one_inta
           (div_ceiling
             (max_list_non_empty linorder_int
               (map (fun i -> abs_int (coeffa zero_int p i)) (upt zero_nata n)))
             (abs_int (coeffa zero_int p (degreea zero_int p))))
         in
        of_int
          (binary_power monoid_mult_int (Int_of_integer (Z.of_int 2))
            (log_ceiling (Int_of_integer (Z.of_int 2)) m)));;

let rec real_alg_2b
  ri p l r =
    (let (pa, (la, ra)) =
       normalize_bounds_1
         (let (la, (ra, _)) =
            tighten_poly_bounds_for_x p zero_rata l r
              (sgn_rata
                (fold_coeffs zero_int
                  (fun a b -> plus_rata (of_int a) (times_rata r b)) p
                  zero_rata))
            in
           (p, (la, ra)))
       in
      Irrational (number_root ri ra, (pa, (la, ra))));;

let rec roots_of_2_main
  p ri cr lrs rais =
    (match lrs with [] -> rais
      | (l, r) :: lrsa ->
        (let c = cr l r in
          (if equal_nata c zero_nata then roots_of_2_main p ri cr lrsa rais
            else (if equal_nata c one_nata
                   then roots_of_2_main p ri cr lrsa
                          (real_alg_2b ri p l r :: rais)
                   else (let m =
                           divide_rata (plus_rata l r)
                             (of_int (Int_of_integer (Z.of_int 2)))
                           in
                          roots_of_2_main p ri cr ((m, r) :: (l, m) :: lrsa)
                            rais)))));;

let rec roots_of_2_irr
  p = (if equal_nata (degreea zero_int p) one_nata
        then [Rational
                (fract
                  (uminus_inta
                    (match coeffs zero_int p with [] -> zero_inta
                      | x :: _ -> x))
                  (coeffa zero_int p one_nata))]
        else (let ri = root_info p in
              let cr = l_r ri in
              let b = root_bound p in
               roots_of_2_main p ri cr [(uminus_rata b, b)] []));;

let rec roots_of_2 p = maps roots_of_2_irr (factors_of_int_poly p);;

let rec roots_of_3_aux
  xa = Abs_real_alg_3_list_x_x_real_alg_2_list (roots_of_2 xa);;

let rec roots_of_3 x = rep_isom (roots_of_3_aux x);;

let rec pos_imaginary_parts_3
  p = remdups_gen equal_3
        (filtera (fun x -> equal_rata (sgn_3 x) one_rata)
          (maps roots_of_3 (root_poly_Im p)));;

let rec filter_list_length
  f p n xs =
    (let ys = filtera p xs in
      (if equal_nata (size_list ys) n then ys
        else filter_list_length f p n (map f ys)));;

let rec to_interval a = Interval (a, a);;

let rec of_int_complex_interval
  x = Complex_Interval
        (to_interval (ratreal (of_int x)), zero_interval zero_real);;

let rec ipoly_complex_interval
  p x = fold_coeffs zero_int
          (fun a b ->
            plus_complex_intervala (of_int_complex_interval a)
              (times_complex_intervala x b))
          p zero_complex_intervala;;

let rec tighten_bounds_2
  = function
    Irrational (n, (p, (l, r))) ->
      (let (la, (ra, _)) =
         tighten_poly_bounds p l r
           (sgn_rata
             (fold_coeffs zero_int
               (fun a b -> plus_rata (of_int a) (times_rata r b)) p zero_rata))
         in
        Irrational (n, (p, (la, ra))))
    | Rational r -> Rational r;;

let rec tighten_bounds_3
  xa = Real_Alg_Invariant (tighten_bounds_2 (rep_real_alg_3 xa));;

let rec real_of_3 x = Real_of (Real_Alg_Quotient x);;

let rec pair_to_complex
  ri = (let (r, i) = ri in Complex (real_of_3 r, real_of_3 i));;

let rec in_interval _A
  y (Interval (lx, ux)) =
    less_eq _A.preorder_order.ord_preorder lx y &&
      less_eq _A.preorder_order.ord_preorder y ux;;

let rec in_complex_interval
  y x = (let Complex_Interval (r, i) = x in
          in_interval order_real (re y) r && in_interval order_real (im y) i);;

let rec root_poly_Re
  p = (let fs =
         coeffs zero_int
           (poly_add
             (factorial_ring_gcd_int, semiring_gcd_mult_normalize_int,
               equal_int)
             p p)
         in
       let k = size_list fs in
        cf_pos_poly
          (poly_of_list (comm_monoid_add_int, equal_int)
            (map (fun (fi, i) ->
                   times_inta fi
                     (binary_power monoid_mult_int (Int_of_integer (Z.of_int 2))
                       i))
              (zip fs (upt zero_nata k)))));;

let rec real_parts_3 p = roots_of_3 (root_poly_Re p);;

let rec get_itvl_2
  = function Irrational (n, (p, (l, r))) -> Interval (ratreal l, ratreal r)
    | Rational r -> (let rr = ratreal r in Interval (rr, rr));;

let rec get_itvl_3 xa = get_itvl_2 (rep_real_alg_3 xa);;

let rec roots_of_real_alg
  x = map (fun a -> Real_Alg_Quotient a) (roots_of_3 x);;

let rec real_roots_of_int_poly
  p = map (fun a -> Real_of a) (roots_of_real_alg p);;

let rec divmod_poly_one_main_list (_A1, _A2)
  q r d n =
    (if equal_nata n zero_nata then (q, r)
      else (let a = hda r in
            let qqq =
              cCons (_A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
                      _A1)
                a q
              in
            let rr =
              tla (if eq _A1 a
                        (zero _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
                    then r
                    else minus_poly_rev_list
                           _A2.ring_1_comm_ring_1.neg_numeral_ring_1.group_add_neg_numeral
                           r (map (times
                                    _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd
                                    a)
                               d))
              in
             divmod_poly_one_main_list (_A1, _A2) qqq rr d
               (minus_nata n one_nata)));;

let rec div_field_poly_impl (_A1, _A2)
  f g = (let cg =
           coeffs
             _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
             g
           in
          (if null cg
            then zero_polya
                   _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
            else (let cf =
                    coeffs
                      _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
                      f
                    in
                  let ilc =
                    inverse _A1.division_ring_field.inverse_division_ring
                      (last cg)
                    in
                  let ch =
                    map (times
                          _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd
                          ilc)
                      cg
                    in
                  let q =
                    fst (divmod_poly_one_main_list
                          (_A2, _A1.idom_divide_field.idom_idom_divide.comm_ring_1_idom)
                          [] (rev cf) (rev ch)
                          (minus_nata (plus_nata one_nata (size_list cf))
                            (size_list cg)))
                    in
                   poly_of_list
                     (_A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.comm_monoid_add_semiring_0,
                       _A2)
                     (map (times
                            _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd
                            ilc)
                       q))));;

let rec of_inta _A
  k = (if equal_inta k zero_inta
        then zero _A.semiring_1_cancel_ring_1.semiring_1_semiring_1_cancel.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
        else (if less_int k zero_inta
               then uminus
                      _A.neg_numeral_ring_1.group_add_neg_numeral.uminus_group_add
                      (of_inta _A (uminus_inta k))
               else (let l =
                       times _A.semiring_1_cancel_ring_1.semiring_1_semiring_1_cancel.semiring_numeral_semiring_1.monoid_mult_semiring_numeral.power_monoid_mult.times_power
                         (numeral _A.neg_numeral_ring_1.numeral_neg_numeral
                           (Bit0 One))
                         (of_inta _A
                           (divide_inta k (Int_of_integer (Z.of_int 2))))
                       in
                     let j = modulo_inta k (Int_of_integer (Z.of_int 2)) in
                      (if equal_inta j zero_inta then l
                        else plus _A.neg_numeral_ring_1.numeral_neg_numeral.semigroup_add_numeral.plus_semigroup_add
                               l (one _A.neg_numeral_ring_1.numeral_neg_numeral.one_numeral)))));;

let rec croots2
  p = (let a = coeffa zero_complex p (nat_of_integer (Z.of_int 2)) in
       let b = coeffa zero_complex p one_nata in
       let c =
         (match coeffs zero_complex p with [] -> zero_complexa | x :: _ -> x) in
       let b2a =
         divide_complexa b
           (times_complexa (numeral numeral_complex (Bit0 One)) a)
         in
       let bac =
         minus_complexa
           (binary_power monoid_mult_complex b2a (nat_of_integer (Z.of_int 2)))
           (divide_complexa c a)
         in
       let e = csqrt bac in
        remdups equal_complex
          [plus_complexa (uminus_complexa b2a) e;
            minus_complexa (uminus_complexa b2a) e]);;

let rec complex_roots_of_int_poly3
  p = (let n = degreea zero_int p in
       let rrts = real_roots_of_int_poly p in
       let nr = size_list rrts in
       let crts = map (fun r -> Complex (r, zero_reala)) rrts in
        (if equal_nata n nr then crts
          else (let nr_crts = minus_nata n nr in
                 (if equal_nata nr_crts (nat_of_integer (Z.of_int 2))
                   then (let pp =
                           div_field_poly_impl (field_real, equal_real)
                             (map_poly zero_int (zero_real, equal_real)
                               (of_inta ring_1_real) p)
                             (prod_list
                               (monoid_mult_poly
                                 (equal_real, comm_semiring_1_real,
                                   semiring_no_zero_divisors_real))
                               (map (fun x ->
                                      pCons (zero_real, equal_real)
(uminus_reala x)
(pCons (zero_real, equal_real) one_reala (zero_polya zero_real)))
                                 rrts))
                           in
                         let cpp =
                           map_poly zero_real (zero_complex, equal_complex)
                             (fun r -> Complex (r, zero_reala)) pp
                           in
                          crts @ croots2 cpp)
                   else (let nr_pos_crts =
                           divide_nata nr_crts (nat_of_integer (Z.of_int 2)) in
                         let rxs = real_parts_3 p in
                         let ixs = pos_imaginary_parts_3 p in
                         let rts =
                           maps (fun rx -> map (fun a -> (rx, a)) ixs) rxs in
                         let crtsa =
                           map pair_to_complex
                             (filter_list_length
                               (map_prod tighten_bounds_3 tighten_bounds_3)
                               (fun (r, i) ->
                                 in_complex_interval zero_complexa
                                   (ipoly_complex_interval p
                                     (Complex_Interval
                                       (get_itvl_3 r, get_itvl_3 i))))
                               nr_pos_crts rts)
                           in
                          crts @ maps (fun x -> [x; cnj x]) crtsa)))));;

let rec roots1 _A
  p = divide _A.division_ring_field.inverse_division_ring.divide_inverse
        (uminus
          _A.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.group_add_neg_numeral.uminus_group_add
          (match
            coeffs
              _A.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
              p
            with [] ->
              zero _A.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
            | x :: _ -> x))
        (coeffa
          _A.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
          p one_nata);;

let rec complex_roots_of_int_poly_all
  p = (let n = degreea zero_int p in
        (if less_eq_nat (nat_of_integer (Z.of_int 3)) n
          then complex_roots_of_int_poly3 p
          else (if equal_nata n one_nata
                 then [roots1 field_complex
                         (map_poly zero_int (zero_complex, equal_complex)
                           (of_inta ring_1_complex) p)]
                 else (if equal_nata n (nat_of_integer (Z.of_int 2))
                        then croots2
                               (map_poly zero_int (zero_complex, equal_complex)
                                 (of_inta ring_1_complex) p)
                        else []))));;

let rec complex_roots_of_int_poly
  p = (let a =
         (if less_eq_nat (nat_of_integer (Z.of_int 3)) (degreea zero_int p)
           then factors_of_int_poly p else [p])
         in
        maps complex_roots_of_int_poly_all a);;

let rec complex_poly
  re im =
    (let i =
       pCons (zero_int, equal_int) one_inta
         (pCons (zero_int, equal_int) zero_inta
           (pCons (zero_int, equal_int) one_inta (zero_polya zero_int)))
       in
      factors_of_int_poly
        (poly_add
          (factorial_ring_gcd_int, semiring_gcd_mult_normalize_int, equal_int)
          re (poly_mult
               (factorial_ring_gcd_int, semiring_gcd_mult_normalize_int,
                 equal_int)
               im i)));;

let rec info_2 = function Rational x -> Inl x
                 | Irrational (n, (p, (l, r))) -> Inr (p, n);;

let rec info_3 xa = info_2 (rep_real_alg_3 xa);;

let rec info_real_alg (Real_Alg_Quotient xa) = info_3 xa;;

let rec min_int_poly_real_alg
  x = (match info_real_alg x with Inl a -> poly_rat a | Inr (p, _) -> p);;

let rec min_int_poly_real (Real_of x) = min_int_poly_real_alg x;;

let rec min_int_poly_complex
  x = (if algebraic_complex x
        then (if equal_reala (im x) zero_reala then min_int_poly_real (re x)
               else the (find (fun f ->
                                membera equal_complex
                                  (complex_roots_of_int_poly f) x)
                          (complex_poly (min_int_poly_real (re x))
                            (min_int_poly_real (im x)))))
        else pCons (zero_int, equal_int) zero_inta
               (pCons (zero_int, equal_int) one_inta (zero_polya zero_int)));;

let rec interval_of_real
  prec x =
    (if is_rat_real x then Interval (x, x)
      else (let n =
              binary_power monoid_mult_int (Int_of_integer (Z.of_int 2)) prec in
            let xa = times_reala x (ratreal (of_int n)) in
             Interval
               (ratreal (fract (floor_real xa) n),
                 ratreal (fract (ceiling floor_ceiling_real xa) n))));;

let rec interval_of_complex
  prec z =
    Complex_Interval
      (interval_of_real prec (re z), interval_of_real prec (im z));;

let rec poly_interval (_A1, _A2, _A3)
  x0 uu = match x0, uu with [], uu -> zero _A3
    | [c], uv -> c
    | c :: v :: va, x ->
        plus _A1 c (times _A2 x (poly_interval (_A1, _A2, _A3) (v :: va) x));;

let rec filter_fun_complex
  p = (let c = coeffs zero_complex p in
        (fun prec ->
          (let cs = map (interval_of_complex prec) c in
            (fun x ->
              in_complex_interval zero_complexa
                (poly_interval
                  (plus_complex_interval, times_complex_interval,
                    zero_complex_interval)
                  cs (interval_of_complex prec x))))));;

let rec all_croots_part1
  xb xc =
    Abs_genuine_roots_aux
      (if equal_nata xb zero_nata ||
            (equal_complexa xc zero_complexa || not (algebraic_complex xc))
        then (one_polya comm_semiring_1_complex,
               ([], (zero_nata,
                      filter_fun_complex (one_polya comm_semiring_1_complex))))
        else (let p = min_int_poly_complex xc in
              let q = poly_nth_root (equal_int, idom_int) xb p in
              let zeros = complex_roots_of_int_poly q in
              let r =
                minus_polya (ab_group_add_complex, equal_complex)
                  (monoma (zero_complex, equal_complex) one_complexa xb)
                  (pCons (zero_complex, equal_complex) xc
                    (zero_polya zero_complex))
                in
               (r, (zeros, (xb, filter_fun_complex r)))));;

let rec all_croots
  n x = (if equal_nata n zero_nata then []
          else (if equal_complexa x zero_complexa then [zero_complexa]
                 else (if algebraic_complex x
                        then genuine_roots_impl field_char_0_complex
                               (all_croots_part1 n x)
                        else failwith
                               "all_croots invoked on non-algebraic number"
                               (fun _ -> all_croots n x))));;

let rec croot
  n x = (if equal_nata n zero_nata then zero_complexa
          else arg_min_list (linorder_prod linorder_real linorder_real)
                 (fun y -> (uminus_reala (re y), uminus_reala (im y)))
                 (all_croots n x));;

let rec croot_ca x = comp croot nat_of_integer x;;

let rec plus_mat _A
  a b = mat (dim_row b) (dim_col b)
          (fun ij -> plus _A (index_mat a ij) (index_mat b ij));;

let rec mat_of_cols
  n cs = mat n (size_list cs) (fun (i, j) -> vec_index (nth cs j) i);;

let rec mat_to_list
  a = map (fun i ->
            map (fun j -> index_mat a (i, j)) (upt zero_nata (dim_col a)))
        (upt zero_nata (dim_row a));;

let rec split_block
  a sr sc =
    (let nr = dim_row a in
     let nc = dim_col a in
     let nr2 = minus_nata nr sr in
     let nc2 = minus_nata nc sc in
     let a1 = mat sr sc (index_mat a) in
     let a2 = mat sr nc2 (fun (i, j) -> index_mat a (i, plus_nata j sc)) in
     let a3 = mat nr2 sc (fun (i, j) -> index_mat a (plus_nata i sr, j)) in
     let a4 =
       mat nr2 nc2 (fun (i, j) -> index_mat a (plus_nata i sr, plus_nata j sc))
       in
      (a1, (a2, (a3, a4))));;

let rec vec_of_list_impl xa = Abs_vec_impl (size_list xa, IArray xa);;

let rec vec_of_list v = Vec_impl (vec_of_list_impl v);;

let rec plus_mata _A
  a b = mat (dim_row b) (dim_col b)
          (fun ij -> plus _A (index_mat a ij) (index_mat b ij));;

let rec char_poly_matrix (_A1, _A2, _A3)
  a = plus_mata
        (plus_poly
          (_A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.comm_monoid_add_semiring_0,
            _A1))
        (smult_mat
          (times_poly
            (_A1, _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.comm_semiring_0_comm_semiring_1,
              _A3))
          (pCons
            (_A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
              _A1)
            (zero _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
            (pCons
              (_A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
                _A1)
              (one _A2.ring_1_comm_ring_1.neg_numeral_ring_1.numeral_neg_numeral.one_numeral)
              (zero_polya
                _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)))
          (one_mat
            ((one_poly
               _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel),
              (zero_poly
                _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero))
            (dim_row a)))
        (map_mat
          (fun aa ->
            pCons (_A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
                    _A1)
              (uminus
                _A2.ring_1_comm_ring_1.neg_numeral_ring_1.group_add_neg_numeral.uminus_group_add
                aa)
              (zero_polya
                _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero))
          a);;

let rec char_poly (_A1, _A2)
  a = det ((equal_poly
             (_A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
               _A1)),
            (idom_divide_poly (_A1, _A2)))
        (char_poly_matrix
          (_A1, _A2.idom_idom_divide.comm_ring_1_idom,
            _A2.idom_idom_divide.semidom_idom.semiring_1_no_zero_divisors_semidom.semiring_no_zero_divisors_semiring_1_no_zero_divisors)
          a);;

let rec shows_prec_nat x = showsp_nat x;;

let rec show_factor
  va = (if equal_nata va zero_nata then []
         else (if equal_nata (minus_nata va one_nata) zero_nata
                then [Chara (false, false, false, true, true, true, true,
                              false)]
                else [Chara (false, false, false, true, true, true, true,
                              false);
                       Chara (false, true, true, true, true, false, true,
                               false)] @
                       shows_prec_nat zero_nata
                         (suc (suc (minus_nata (minus_nata va one_nata)
                                     one_nata)))
                         []));;

let rec show_coeff_factor (_A1, _A2, _A3)
  c n = (if equal_nata n zero_nata then shows_prec _A3 zero_nata c []
          else (if eq _A2 c (one _A1) then show_factor n
                 else shows_prec _A3 zero_nata c [] @ show_factor n));;

let rec show_poly_main (_A1, _A2, _A3, _A4)
  uu x1 = match uu, x1 with
    uu, [] -> [Chara (false, false, false, false, true, true, false, false)]
    | n, [c] -> show_coeff_factor (_A1, _A3, _A4) c n
    | n, c :: v :: va ->
        (if eq _A3 c (zero _A2)
          then show_poly_main (_A1, _A2, _A3, _A4) (suc n) (v :: va)
          else show_coeff_factor (_A1, _A3, _A4) c n @
                 [Chara (false, false, false, false, false, true, false, false);
                   Chara (true, true, false, true, false, true, false, false);
                   Chara (false, false, false, false, false, true, false,
                           false)] @
                   show_poly_main (_A1, _A2, _A3, _A4) (suc n) (v :: va));;

let rec show_poly (_A1, _A2, _A3, _A4)
  p = show_poly_main (_A1, _A2, _A3, _A4) zero_nata (coeffs _A2 p);;

let rec real_alg_of_real (Real_of x) = x;;

let rec convert_info
  = function Inl q -> Rat_Info q
    | Inr (f, n) ->
        (if equal_nata (degreea zero_int f) (nat_of_integer (Z.of_int 2))
          then (let a = coeffa zero_int f (nat_of_integer (Z.of_int 2)) in
                let b = coeffa zero_int f one_nata in
                let c =
                  (match coeffs zero_int f with [] -> zero_inta | x :: _ -> x)
                  in
                let b2a =
                  fract (uminus_inta b)
                    (times_inta (Int_of_integer (Z.of_int 2)) a)
                  in
                let below =
                  fract (minus_inta (times_inta b b)
                          (times_inta
                            (times_inta (Int_of_integer (Z.of_int 4)) a) c))
                    (times_inta (times_inta (Int_of_integer (Z.of_int 4)) a) a)
                  in
                 Sqrt_Info
                   (b2a, (if equal_nata n one_nata then uminus_rata below
                           else below)))
          else Real_Alg_Info (f, n));;

let rec real_alg_show_info x = convert_info (info_real_alg x);;

let rec showsp_rat
  p x = (let (d, n) = quotient_of x in
          (if equal_inta n one_inta then showsp_int p d
            else comp (comp (showsp_int p d)
                        (shows_string
                          [Chara (true, true, true, true, false, true, false,
                                   false)]))
                   (showsp_int p n)));;

let rec shows_prec_rat x = showsp_rat x;;

let rec showsp_poly (_A1, _A2, _A3, _A4)
  p x = shows_string (show_poly (_A1, _A2, _A3, _A4) x);;

let rec shows_prec_poly (_A1, _A2, _A3, _A4)
  p x = showsp_poly (_A1, _A2, _A3, _A4) p x;;

let rec show_rai_info
  fl x1 = match fl, x1 with fl, Rat_Info r -> shows_prec_rat zero_nata r []
    | fl, Sqrt_Info (r, sq) ->
        (let sqrt =
           [Chara (true, true, false, false, true, true, true, false);
             Chara (true, false, false, false, true, true, true, false);
             Chara (false, true, false, false, true, true, true, false);
             Chara (false, false, true, false, true, true, true, false);
             Chara (false, false, false, true, false, true, false, false)] @
             shows_prec_rat zero_nata (abs_rata sq) [] @
               [Chara (true, false, false, true, false, true, false, false)]
           in
          (if equal_rata r zero_rata
            then (if less_rat sq zero_rata
                   then [Chara (false, false, false, false, false, true, false,
                                 false);
                          Chara (true, false, true, true, false, true, false,
                                  false)]
                   else []) @
                   sqrt
            else [Chara (false, false, false, true, false, true, false,
                          false)] @
                   shows_prec_rat zero_nata r [] @
                     (if less_rat sq zero_rata
                       then [Chara (true, false, true, true, false, true, false,
                                     false)]
                       else [Chara (true, true, false, true, false, true, false,
                                     false)]) @
                       sqrt @
                         [Chara (true, false, false, true, false, true, false,
                                  false)]))
    | fl, Real_Alg_Info (p, n) ->
        [Chara (false, false, false, true, false, true, false, false);
          Chara (false, true, false, false, true, true, true, false);
          Chara (true, true, true, true, false, true, true, false);
          Chara (true, true, true, true, false, true, true, false);
          Chara (false, false, true, false, true, true, true, false);
          Chara (false, false, false, false, false, true, false, false);
          Chara (true, true, false, false, false, true, false, false)] @
          shows_prec_nat zero_nata n [] @
            [Chara (false, false, false, false, false, true, false, false);
              Chara (true, true, true, true, false, true, true, false);
              Chara (false, true, true, false, false, true, true, false);
              Chara (false, false, false, false, false, true, false, false)] @
              shows_prec_poly (one_int, zero_int, equal_int, show_int) zero_nata
                p [] @
                [Chara (false, false, true, true, false, true, false, false);
                  Chara (false, false, false, false, false, true, false, false);
                  Chara (true, false, false, true, false, true, true, false);
                  Chara (false, true, true, true, false, true, true, false);
                  Chara (false, false, false, false, false, true, false, false);
                  Chara (false, false, false, true, false, true, false,
                          false)] @
                  shows_prec_int zero_nata fl [] @
                    [Chara (false, false, true, true, false, true, false,
                             false)] @
                      shows_prec_int zero_nata (plus_inta fl one_inta) [] @
                        [Chara (true, false, false, true, false, true, false,
                                 false);
                          Chara (true, false, false, true, false, true, false,
                                  false)];;

let rec show_real_alg
  x = show_rai_info (floor_real_alg x) (real_alg_show_info x);;

let rec show_real x = comp show_real_alg real_alg_of_real x;;

let rec coeffs_int x = coeffs zero_integer x;;

let rec mat_equal_impl _A
  xa xc =
    (let (nr1, (nc1, m1)) = rep_mat_impl xa in
      (fun (nr2, (nc2, m2)) ->
        equal_nata nr1 nr2 &&
          (equal_nata nc1 nc2 && equal_iarraya (equal_iarray _A) m1 m2)))
      (rep_mat_impl xc);;

let rec equal_mat _A (Mat_impl m1) (Mat_impl m2) = mat_equal_impl _A m1 m2;;

let rec eliminate_entries_gen
  minus times v a i j =
    mat (dim_row a) (dim_col a)
      (fun (ia, ja) ->
        (if not (equal_nata ia i)
          then minus (index_mat a (ia, ja)) (times (v ia) (index_mat a (i, ja)))
          else index_mat a (ia, ja)));;

let rec gauss_jordan_main (_A1, _A2)
  a b i j =
    (let nr = dim_row a in
     let nc = dim_col a in
      (if less_nat i nr && less_nat j nc
        then (let aij = index_mat a (i, j) in
               (if eq _A2 aij
                     (zero _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
                 then (match
                        maps (fun ia ->
                               (if not (eq _A2 (index_mat a (ia, j))
 (zero _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero))
                                 then [ia] else []))
                          (upt (suc i) nr)
                        with [] -> gauss_jordan_main (_A1, _A2) a b i (suc j)
                        | ia :: _ ->
                          gauss_jordan_main (_A1, _A2) (mat_swaprows i ia a)
                            (mat_swaprows i ia b) i j)
                 else (if eq _A2 aij
                            (one _A1.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.numeral_neg_numeral.one_numeral)
                        then (let v = (fun ia -> index_mat a (ia, j)) in
                               gauss_jordan_main (_A1, _A2)
                                 (eliminate_entries_gen
                                   (minus
                                     _A1.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.group_add_neg_numeral.minus_group_add)
                                   (times
                                     _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd)
                                   v a i j)
                                 (eliminate_entries_gen
                                   (minus
                                     _A1.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.group_add_neg_numeral.minus_group_add)
                                   (times
                                     _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd)
                                   v b i j)
                                 (suc i) (suc j))
                        else (let iaij =
                                inverse
                                  _A1.division_ring_field.inverse_division_ring
                                  aij
                                in
                              let aa =
                                mat_multrow_gen
                                  (times
                                    _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd)
                                  i iaij a
                                in
                              let ba =
                                mat_multrow_gen
                                  (times
                                    _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd)
                                  i iaij b
                                in
                              let v = (fun ia -> index_mat aa (ia, j)) in
                               gauss_jordan_main (_A1, _A2)
                                 (eliminate_entries_gen
                                   (minus
                                     _A1.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.group_add_neg_numeral.minus_group_add)
                                   (times
                                     _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd)
                                   v aa i j)
                                 (eliminate_entries_gen
                                   (minus
                                     _A1.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.group_add_neg_numeral.minus_group_add)
                                   (times
                                     _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd)
                                   v ba i j)
                                 (suc i) (suc j)))))
        else (a, b)));;

let rec gauss_jordan (_A1, _A2)
  a b = gauss_jordan_main (_A1, _A2) a b zero_nata zero_nata;;

let rec mat_inverse (_A1, _A2)
  a = (if equal_nata (dim_row a) (dim_col a)
        then (let one =
                one_mat
                  (_A1.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.numeral_neg_numeral.one_numeral,
                    _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
                  (dim_row a)
                in
              let (b, c) = gauss_jordan (_A1, _A2) a one in
               (if equal_mat _A2 b one then Some c else None))
        else None);;

let rec mat_inv_ca x = mat_inverse (field_complex, equal_complex) x;;

let rec lcms _A
  xs = fold (lcma _A.comm_monoid_gcd_semiring_gcd.gcd_comm_monoid_gcd) xs
         (one _A.comm_monoid_gcd_semiring_gcd.gcd_comm_monoid_gcd.one_gcd);;

let rec int_of_rat x = fst (quotient_of x);;

let rec to_int _A x = int_of_rat (to_rat _A x);;

let rec char_matrix _A
  a e = plus_mata
          _A.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.numeral_neg_numeral.semigroup_add_numeral.plus_semigroup_add
          a (smult_mat
              _A.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd
              (uminus
                _A.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.group_add_neg_numeral.uminus_group_add
                e)
              (one_mat
                (_A.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.numeral_neg_numeral.one_numeral,
                  _A.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
                (dim_row a)));;

let rec conjugate_vec _A
  v = vec (dim_vec v) (fun i -> conjugate _A (vec_index v i));;

let rec plus_vec _A
  v_1 v_2 =
    vec (dim_vec v_2) (fun i -> plus _A (vec_index v_1 i) (vec_index v_2 i));;

let rec adjuster _A
  n w x2 = match n, w, x2 with
    n, w, [] ->
      zero_vec
        _A.field_conjugatable_field.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
        n
    | n, w, u :: us ->
        plus_vec
          _A.field_conjugatable_field.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.numeral_neg_numeral.semigroup_add_numeral.plus_semigroup_add
          (smult_vec
            _A.field_conjugatable_field.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd
            (divide
              _A.field_conjugatable_field.division_ring_field.inverse_division_ring.divide_inverse
              (uminus
                _A.field_conjugatable_field.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.group_add_neg_numeral.uminus_group_add
                (scalar_prod
                  _A.field_conjugatable_field.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1
                  w (conjugate_vec
                      _A.conjugatable_ring_conjugatable_field.conjugate_conjugatable_ring
                      u)))
              (scalar_prod
                _A.field_conjugatable_field.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1
                u (conjugate_vec
                    _A.conjugatable_ring_conjugatable_field.conjugate_conjugatable_ring
                    u)))
            u)
          (adjuster _A n w us);;

let rec four_block_mat
  a b c d =
    (let nra = dim_row a in
     let nrd = dim_row d in
     let nca = dim_col a in
     let ncd = dim_col d in
      mat (plus_nata nra nrd) (plus_nata nca ncd)
        (fun (i, j) ->
          (if less_nat i nra
            then (if less_nat j nca then index_mat a (i, j)
                   else index_mat b (i, minus_nata j nca))
            else (if less_nat j nca then index_mat c (minus_nata i nra, j)
                   else index_mat d (minus_nata i nra, minus_nata j nca)))));;

let rec of_integer_ca
  x = comp (of_inta ring_1_complex) (fun a -> Int_of_integer a) x;;

let rec basis_completion (_A1, _A2)
  v = (let n = dim_vec v in
       let drop_index =
         hda (maps (fun i ->
                     (if not (eq _A1 (vec_index v i)
                               (zero _A2.semiring_1_cancel_ring_1.semiring_1_semiring_1_cancel.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero))
                       then [i] else []))
               (upt zero_nata n))
         in
       let a =
         maps (fun i ->
                (if not (equal_nata i drop_index)
                  then [unit_vec
                          _A2.semiring_1_cancel_ring_1.semiring_1_semiring_1_cancel.zero_neq_one_semiring_1
                          n i]
                  else []))
           (upt zero_nata n)
         in
        v :: a);;

let rec vec_inv _A
  v = smult_vec
        _A.field_conjugatable_field.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd
        (divide
          _A.field_conjugatable_field.division_ring_field.inverse_division_ring.divide_inverse
          (one _A.field_conjugatable_field.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.numeral_neg_numeral.one_numeral)
          (scalar_prod
            _A.field_conjugatable_field.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1
            v (conjugate_vec
                _A.conjugatable_ring_conjugatable_field.conjugate_conjugatable_ring
                v)))
        (conjugate_vec
          _A.conjugatable_ring_conjugatable_field.conjugate_conjugatable_ring
          v);;

let rec corthogonal_inv _A
  a = mat_of_rows (dim_row a) (map (vec_inv _A) (cols a));;

let rec find_base_vector (_A1, _A2)
  a = (let pp =
         pivot_positions_gen _A1
           (zero _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
           a
         in
       let cands =
         filtera (fun j -> not (membera equal_nat (map snd pp) j))
           (upt zero_nata (dim_col a))
         in
        non_pivot_base_gen
          (uminus
            _A2.ring_1_comm_ring_1.neg_numeral_ring_1.group_add_neg_numeral.uminus_group_add)
          (zero _A2.comm_semiring_1_cancel_comm_ring_1.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
          (one _A2.ring_1_comm_ring_1.neg_numeral_ring_1.numeral_neg_numeral.one_numeral)
          a pp (hda cands));;

let rec find_eigenvector (_A1, _A2)
  a e = find_base_vector
          (_A2, _A1.idom_divide_field.idom_idom_divide.comm_ring_1_idom)
          (fst (gauss_jordan (_A1, _A2) (char_matrix _A1 a e)
                 (zero_mat
                   _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
                   (dim_row a) zero_nata)));;

let rec gram_schmidt_sub2 _A
  n us x2 = match n, us, x2 with n, us, [] -> []
    | n, us, w :: ws ->
        (let u =
           plus_vec
             _A.field_conjugatable_field.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.numeral_neg_numeral.semigroup_add_numeral.plus_semigroup_add
             (adjuster _A n w us) w
           in
          u :: gram_schmidt_sub2 _A n (u :: us) ws);;

let rec gram_schmidt _A n ws = gram_schmidt_sub2 _A n [] ws;;

let rec schur_decomposition (_A1, _A2)
  a x1 = match a, x1 with
    a, [] ->
      (a, (one_mat
             (_A1.field_conjugatable_field.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.numeral_neg_numeral.one_numeral,
               _A1.field_conjugatable_field.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
             (dim_row a),
            one_mat
              (_A1.field_conjugatable_field.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.numeral_neg_numeral.one_numeral,
                _A1.field_conjugatable_field.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
              (dim_row a)))
    | a, e :: es ->
        (let n = dim_row a in
         let n1 = minus_nata n one_nata in
         let v = find_eigenvector (_A1.field_conjugatable_field, _A2) a e in
         let ws =
           gram_schmidt _A1 n
             (basis_completion
               (_A2, _A1.field_conjugatable_field.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1)
               v)
           in
         let w = mat_of_cols n ws in
         let wa = corthogonal_inv _A1 w in
         let aa =
           times_mat
             _A1.field_conjugatable_field.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1
             (times_mat
               _A1.field_conjugatable_field.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1
               wa a)
             w
           in
         let (a1, (a2, (a0, a3))) = split_block aa one_nata one_nata in
         let (b, (p, q)) = schur_decomposition (_A1, _A2) a3 es in
         let z_row =
           zero_mat
             _A1.field_conjugatable_field.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
             one_nata n1
           in
         let z_col =
           zero_mat
             _A1.field_conjugatable_field.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
             n1 one_nata
           in
         let one_1 =
           one_mat
             (_A1.field_conjugatable_field.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.numeral_neg_numeral.one_numeral,
               _A1.field_conjugatable_field.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
             one_nata
           in
          (four_block_mat a1
             (times_mat
               _A1.field_conjugatable_field.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1
               a2 p)
             a0 b,
            (times_mat
               _A1.field_conjugatable_field.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1
               w (four_block_mat one_1 z_row z_col p),
              times_mat
                _A1.field_conjugatable_field.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1
                (four_block_mat one_1 z_row z_col q) wa)));;

let rec schur_decomp
  a es =
    (let (b, (p, q)) =
       schur_decomposition (conjugatable_field_complex, equal_complex)
         (map_mat of_integer_ca a) es
       in
      (b, (p, q)));;

let imaginary_unit : complex = Complex (zero_reala, one_reala);;

let rec char_poly_int x = char_poly (equal_integer, idom_divide_integer) x;;

let rec upper_triangular (_A1, _A2)
  a = all_interval_nat
        (fun i ->
          all_interval_nat (fun j -> eq _A2 (index_mat a (i, j)) (zero _A1))
            zero_nata i)
        zero_nata (dim_row a);;

let rec common_denom
  xs = (let nds = map quotient_of xs in
        let denom = lcms semiring_gcd_int (map snd nds) in
        let a = map (fun (n, a) -> divide_inta (times_inta n denom) a) nds in
         (denom, a));;

let rec is_int_rat x = equal_inta (snd (quotient_of x)) one_inta;;

let rec gauss_jordan_single (_A1, _A2)
  a = fst (gauss_jordan (_A1, _A2) a
            (zero_mat
              _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
              (dim_row a) zero_nata));;

let rec kernel_dim (_A1, _A2)
  a = minus_nata (dim_col a)
        (size_list
          (pivot_positions_gen _A2
            (zero _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
            (gauss_jordan_single (_A1, _A2) a)));;

let rec show_complex
  x = (let r = re x in
       let i = im x in
        (if equal_reala i zero_reala then show_real r
          else (if equal_reala r zero_reala
                 then show_real i @
                        [Chara (true, false, false, true, false, true, true,
                                 false)]
                 else [Chara (false, false, false, true, false, true, false,
                               false)] @
                        show_real r @
                          [Chara (true, true, false, true, false, true, false,
                                   false)] @
                            show_real i @
                              [Chara (true, false, false, true, false, true,
                                       true, false);
                                Chara (true, false, false, true, false, true,
false, false)])));;

let rec basic_div_exp _A
  x y n =
    divide _A.semidom_divide_idom_divide.divide_semidom_divide
      (binary_power
        _A.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_numeral_semiring_1.monoid_mult_semiring_numeral
        x n)
      (binary_power
        _A.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_numeral_semiring_1.monoid_mult_semiring_numeral
        y (minus_nata n one_nata));;

let rec rat_to_int_poly
  p = (let ais = coeffs zero_rat p in
       let d = fst (common_denom ais) in
        (d, map_poly zero_rat (zero_int, equal_int)
              (fun x ->
                (let (pa, a) = quotient_of x in
                  divide_inta (times_inta pa d) a))
              p));;

let rec showsp_complex p x y = show_complex x @ y;;

let rec mat_addcol _A
  aa k l a =
    mat (dim_row a) (dim_col a)
      (fun (i, j) ->
        (if equal_nata k j
          then plus _A.semiring_numeral_semiring_1.numeral_semiring_numeral.semigroup_add_numeral.plus_semigroup_add
                 (times
                   _A.semiring_numeral_semiring_1.monoid_mult_semiring_numeral.power_monoid_mult.times_power
                   aa (index_mat a (i, l)))
                 (index_mat a (i, j))
          else index_mat a (i, j)));;

let rec mat_multcol _A
  k aa a =
    mat (dim_row a) (dim_col a)
      (fun (i, j) ->
        (if equal_nata k j
          then times _A.semiring_numeral_semiring_1.monoid_mult_semiring_numeral.power_monoid_mult.times_power
                 aa (index_mat a (i, j))
          else index_mat a (i, j)));;

let rec of_rat _A
  p = (let (a, b) = quotient_of p in
        divide
          _A.field_field_char_0.division_ring_field.inverse_division_ring.divide_inverse
          (of_inta _A.ring_char_0_field_char_0.ring_1_ring_char_0 a)
          (of_inta _A.ring_char_0_field_char_0.ring_1_ring_char_0 b));;

let rec showsp_real_alg p x y = show_real_alg x @ y;;

let rec mat_swapcols
  k l a =
    mat (dim_row a) (dim_col a)
      (fun (i, j) ->
        (if equal_nata k j then index_mat a (i, l)
          else (if equal_nata l j then index_mat a (i, k)
                 else index_mat a (i, j))));;

let rec dim_gen_eigenspace (_A1, _A2)
  a ev k =
    kernel_dim (_A1, _A2)
      (pow_mat
        _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1
        (char_matrix _A1 a ev) k);;

let rec dim_gen_eigenspace_ca
  x = dim_gen_eigenspace (field_complex, equal_complex) x;;

let rec gauss_jordan_single_ca
  x = gauss_jordan_single (field_complex, equal_complex) x;;

let rec swap_cols_rows k l a = mat_swaprows k l (mat_swapcols k l a);;

let rec add_col_sub_row _A
  aa k l a =
    mat_addrow_gen
      (plus _A.neg_numeral_ring_1.numeral_neg_numeral.semigroup_add_numeral.plus_semigroup_add)
      (times
        _A.semiring_1_cancel_ring_1.semiring_1_semiring_1_cancel.semiring_numeral_semiring_1.monoid_mult_semiring_numeral.power_monoid_mult.times_power)
      (uminus _A.neg_numeral_ring_1.group_add_neg_numeral.uminus_group_add aa) k
      l (mat_addcol _A.semiring_1_cancel_ring_1.semiring_1_semiring_1_cancel aa
          l k a);;

let rec
  rep_nat_x_idom_x_x_a_rf_poly_prod_list_x_idom_x_x_a_prod_x_x_nat_x_idom_x_x_a_poly_prod_list_x_idom_x_x_a_prod
  _A (Abs_nat_x_idom_x_x_a_rf_poly_prod_list_x_idom_x_x_a_prod_x_x_nat_x_idom_x_x_a_poly_prod_list_x_idom_x_x_a_prod
       x)
       = x;;

let rec sel12_aux _A
  xa = Abs_nat_x_idom_x_x_a_rf_poly_prod_list_x_x_nat_x_idom_x_x_a_poly_prod_list
         (let (_, x2) =
            rep_nat_x_idom_x_x_a_rf_poly_prod_list_x_idom_x_x_a_prod_x_x_nat_x_idom_x_x_a_poly_prod_list_x_idom_x_x_a_prod
              _A xa
            in
           x2);;

let rec
  rep_nat_x_idom_x_x_a_rf_poly_prod_list_x_x_nat_x_idom_x_x_a_poly_prod_list
  _A (Abs_nat_x_idom_x_x_a_rf_poly_prod_list_x_x_nat_x_idom_x_x_a_poly_prod_list
       x)
       = x;;

let rec sel22a _A
  xa = Abs_nat_x_idom_x_x_a_rf_poly_prod_list_x_x_nat_x_idom_x_x_a_poly_prod_list
         (match
           rep_nat_x_idom_x_x_a_rf_poly_prod_list_x_x_nat_x_idom_x_x_a_poly_prod_list
             _A xa
           with [] ->
             rep_nat_x_idom_x_x_a_rf_poly_prod_list_x_x_nat_x_idom_x_x_a_poly_prod_list
               _A (failwith "undefined")
           | _ :: x22 -> x22);;

let rec rep_rf_poly _A (Abs_rf_poly x) = x;;

let rec sel21_aux _A
  xa = Abs_nat_x_idom_x_x_a_rf_poly_prod_x_x_nat_x_idom_x_x_a_poly_prod
         (match
           rep_nat_x_idom_x_x_a_rf_poly_prod_list_x_x_nat_x_idom_x_x_a_poly_prod_list
             _A xa
           with [] -> map_prod (rep_rf_poly _A) id (failwith "undefined")
           | x21 :: _ -> x21);;

let rec rep_nat_x_idom_x_x_a_rf_poly_prod_x_x_nat_x_idom_x_x_a_poly_prod _A
  (Abs_nat_x_idom_x_x_a_rf_poly_prod_x_x_nat_x_idom_x_x_a_poly_prod x) = x;;

let rec sel12 _A
  xa = (let (_, x2) =
          rep_nat_x_idom_x_x_a_rf_poly_prod_x_x_nat_x_idom_x_x_a_poly_prod _A xa
          in
         x2);;

let rec sel11 _A
  xa = Abs_rf_poly
         (let (x1, _) =
            rep_nat_x_idom_x_x_a_rf_poly_prod_x_x_nat_x_idom_x_x_a_poly_prod _A
              xa
            in
           x1);;

let rec rep_isomb _A x = (sel11 _A x, sel12 _A x);;

let rec sel21b _A x = rep_isomb _A (sel21_aux _A x);;

let rec dis1b _A
  xa = (match
         rep_nat_x_idom_x_x_a_rf_poly_prod_list_x_x_nat_x_idom_x_x_a_poly_prod_list
           _A xa
         with [] -> true | _ :: _ -> false);;

let rec rep_isomc _A
  x = (if dis1b _A x then [] else sel21b _A x :: rep_isomc _A (sel22a _A x));;

let rec sel12a _A x = rep_isomc _A (sel12_aux _A x);;

let rec sel11a _A
  xa = (let (x1, _) =
          rep_nat_x_idom_x_x_a_rf_poly_prod_list_x_idom_x_x_a_prod_x_x_nat_x_idom_x_x_a_poly_prod_list_x_idom_x_x_a_prod
            _A xa
          in
         x1);;

let rec rep_isomd _A x = (sel11a _A x, sel12a _A x);;

let rec rep_rf_polys _A (Abs_rf_polys x) = x;;

let rec yun_rf_aux _A
  x = Abs_nat_x_idom_x_x_a_rf_poly_prod_list_x_idom_x_x_a_prod_x_x_nat_x_idom_x_x_a_poly_prod_list_x_idom_x_x_a_prod
        (rep_rf_polys _A x);;

let rec yun_rf _A x = rep_isomd _A (yun_rf_aux _A x);;

let rec resultant_impl_rec_basic (_A1, _A2)
  gi_1 gi ni_1 d1_1 hi_2 =
    (let ni =
       degreea
         _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
         gi
       in
     let pmod =
       pseudo_mod
         (_A1, _A2.idom_idom_divide.comm_ring_1_idom,
           _A2.idom_idom_divide.semidom_idom.semiring_1_no_zero_divisors_semidom)
         gi_1 gi
       in
      (if is_zero
            _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
            pmod
        then (if equal_nata ni zero_nata
               then (let d1 = minus_nata ni_1 ni in
                     let gia =
                       coeffa
                         _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
                         gi (degreea
                              _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
                              gi)
                       in
                      (if equal_nata d1 one_nata then gia
                        else (let gi_1a =
                                coeffa
                                  _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
                                  gi_1
                                  (degreea
                                    _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
                                    gi_1)
                                in
                              let hi_1 =
                                (if equal_nata d1_1 one_nata then gi_1a
                                  else basic_div_exp _A2 gi_1a hi_2 d1_1)
                                in
                               basic_div_exp _A2 gia hi_1 d1)))
               else zero _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
        else (let d1 = minus_nata ni_1 ni in
              let gi_1a =
                coeffa
                  _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
                  gi_1
                  (degreea
                    _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
                    gi_1)
                in
              let hi_1 =
                (if equal_nata d1_1 one_nata then gi_1a
                  else basic_div_exp _A2 gi_1a hi_2 d1_1)
                in
              let divisor =
                (if equal_nata d1 one_nata
                  then times _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd
                         gi_1a hi_1
                  else (if dvd (equal_nat, semidom_modulo_nat)
                             (nat_of_integer (Z.of_int 2)) d1
                         then times _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd
                                (uminus
                                  _A2.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.group_add_neg_numeral.uminus_group_add
                                  gi_1a)
                                (binary_power
                                  _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_numeral_semiring_1.monoid_mult_semiring_numeral
                                  hi_1 d1)
                         else times _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd
                                gi_1a
                                (binary_power
                                  _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_numeral_semiring_1.monoid_mult_semiring_numeral
                                  hi_1 d1)))
                in
              let gi_p1 = sdiv_poly (_A1, _A2) pmod divisor in
               resultant_impl_rec_basic (_A1, _A2) gi gi_p1 ni d1 hi_1)));;

let rec resultant_impl_start_basic (_A1, _A2)
  g1 g2 =
    (let pmod =
       pseudo_mod
         (_A1, _A2.idom_idom_divide.comm_ring_1_idom,
           _A2.idom_idom_divide.semidom_idom.semiring_1_no_zero_divisors_semidom)
         g1 g2
       in
     let n2 =
       degreea
         _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
         g2
       in
     let n1 =
       degreea
         _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
         g1
       in
     let g2a =
       coeffa
         _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
         g2 (degreea
              _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
              g2)
       in
     let d1 = minus_nata n1 n2 in
      (if is_zero
            _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
            pmod
        then (if equal_nata n2 zero_nata
               then (if equal_nata d1 zero_nata
                      then one _A2.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.numeral_neg_numeral.one_numeral
                      else (if equal_nata d1 one_nata then g2a
                             else binary_power
                                    _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_numeral_semiring_1.monoid_mult_semiring_numeral
                                    g2a d1))
               else zero _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
        else (let g3 =
                (if dvd (equal_nat, semidom_modulo_nat)
                      (nat_of_integer (Z.of_int 2)) d1
                  then uminus_polya
                         _A2.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.ring_ring_1.ab_group_add_ring
                         pmod
                  else pmod)
                in
              let n3 =
                degreea
                  _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
                  g3
                in
              let pmoda =
                pseudo_mod
                  (_A1, _A2.idom_idom_divide.comm_ring_1_idom,
                    _A2.idom_idom_divide.semidom_idom.semiring_1_no_zero_divisors_semidom)
                  g2 g3
                in
               (if is_zero
                     _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
                     pmoda
                 then (if equal_nata n3 zero_nata
                        then (let d2 = minus_nata n2 n3 in
                              let g3a =
                                coeffa
                                  _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
                                  g3 (degreea
                                       _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
                                       g3)
                                in
                               (if equal_nata d2 one_nata then g3a
                                 else basic_div_exp _A2 g3a
(if equal_nata d1 one_nata then g2a
  else binary_power
         _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_numeral_semiring_1.monoid_mult_semiring_numeral
         g2a d1)
d2))
                        else zero _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
                 else (let h2 =
                         (if equal_nata d1 one_nata then g2a
                           else binary_power
                                  _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_numeral_semiring_1.monoid_mult_semiring_numeral
                                  g2a d1)
                         in
                       let d2 = minus_nata n2 n3 in
                       let divisor =
                         (if equal_nata d2 one_nata
                           then times _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd
                                  g2a h2
                           else (if dvd (equal_nat, semidom_modulo_nat)
                                      (nat_of_integer (Z.of_int 2)) d2
                                  then times
 _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd
 (uminus
   _A2.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.group_add_neg_numeral.uminus_group_add
   g2a)
 (binary_power
   _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_numeral_semiring_1.monoid_mult_semiring_numeral
   h2 d2)
                                  else times
 _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd
 g2a (binary_power
       _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_numeral_semiring_1.monoid_mult_semiring_numeral
       h2 d2)))
                         in
                       let g4 = sdiv_poly (_A1, _A2) pmoda divisor in
                        resultant_impl_rec_basic (_A1, _A2) g3 g4 n3 d2
                          h2)))));;

let rec resultant_impl_main_basic (_A1, _A2)
  g1 g2 =
    (if is_zero
          _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
          g2
      then (if equal_nata
                 (degreea
                   _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
                   g1)
                 zero_nata
             then one _A2.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.numeral_neg_numeral.one_numeral
             else zero _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)
      else resultant_impl_start_basic (_A1, _A2) g1 g2);;

let rec resultant_impl_basic (_A1, _A2)
  f g = (if less_eq_nat
              (size_list
                (coeffs
                  _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
                  g))
              (size_list
                (coeffs
                  _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
                  f))
          then resultant_impl_main_basic (_A1, _A2) f g
          else (let res = resultant_impl_main_basic (_A1, _A2) g f in
                 (if dvd (equal_nat, semidom_modulo_nat)
                       (nat_of_integer (Z.of_int 2))
                       (degreea
                         _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
                         f) ||
                       dvd (equal_nat, semidom_modulo_nat)
                         (nat_of_integer (Z.of_int 2))
                         (degreea
                           _A2.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
                           g)
                   then res
                   else uminus
                          _A2.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.group_add_neg_numeral.uminus_group_add
                          res)));;

let rec mult_col_div_row _A
  aa k a =
    mat_multrow_gen
      (times
        _A.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_monoid_mult_comm_semiring_1.dvd_comm_monoid_mult.times_dvd)
      k (inverse _A.division_ring_field.inverse_division_ring aa)
      (mat_multcol
        _A.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1
        k aa a);;

let rec step_1_main (_A1, _A2)
  n i j a =
    (if less_eq_nat n j then a
      else (if equal_nata i zero_nata
             then step_1_main (_A1, _A2) n (plus_nata j one_nata)
                    (plus_nata j one_nata) a
             else (let ia = minus_nata i one_nata in
                   let ev_left = index_mat a (ia, ia) in
                   let ev_below = index_mat a (j, j) in
                   let aij = index_mat a (ia, j) in
                   let aa =
                     (if not (eq _A2 ev_left ev_below) &&
                           not (eq _A2 aij
                                 (zero _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero))
                       then add_col_sub_row
                              _A1.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1
                              (divide
                                _A1.division_ring_field.inverse_division_ring.divide_inverse
                                aij (minus
                                      _A1.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.group_add_neg_numeral.minus_group_add
                                      ev_below ev_left))
                              ia j a
                       else a)
                     in
                    step_1_main (_A1, _A2) n ia j aa)));;

let rec step_1 (_A1, _A2)
  a = step_1_main (_A1, _A2) (dim_row a) zero_nata zero_nata a;;

let rec swap_cols_rows_block
  i j a =
    (if less_nat i j then swap_cols_rows_block (suc i) j (swap_cols_rows i j a)
      else a);;

let rec lookup_ev _A
  ev i a =
    (if equal_nata i zero_nata then None
      else (if eq _A (index_mat a
                       (minus_nata i one_nata, minus_nata i one_nata))
                 ev
             then Some (minus_nata i one_nata)
             else lookup_ev _A ev (minus_nata i one_nata) a));;

let rec step_2_main (_A1, _A2)
  n j a =
    (if less_eq_nat n j then a
      else (let ev = index_mat a (j, j) in
            let aa =
              (match lookup_ev _A2 ev j a with None -> a
                | Some i -> swap_cols_rows_block (suc i) j a)
              in
             step_2_main (_A1, _A2) n (suc j) aa));;

let rec step_2 (_A1, _A2) a = step_2_main (_A1, _A2) (dim_row a) zero_nata a;;

let rec find_largest_block
  block x1 = match block, x1 with block, [] -> block
    | (m_start, m_end), (i_start, i_end) :: blocks ->
        (if less_eq_nat (minus_nata m_end m_start) (minus_nata i_end i_start)
          then find_largest_block (i_start, i_end) blocks
          else find_largest_block (m_start, m_end) blocks);;

let rec identify_block (_A1, _A2)
  a i = (if equal_nata i zero_nata then zero_nata
          else (if eq _A2
                     (index_mat a
                       (minus_nata i one_nata, suc (minus_nata i one_nata)))
                     (one _A1)
                 then identify_block (_A1, _A2) a (minus_nata i one_nata)
                 else suc (minus_nata i one_nata)));;

let rec identify_blocks_main (_A1, _A2)
  a i_end lista =
    (if equal_nata i_end zero_nata then lista
      else (let i_begin =
              identify_block
                (_A2.neg_numeral_ring_1.numeral_neg_numeral.one_numeral, _A1) a
                (minus_nata i_end one_nata)
              in
             identify_blocks_main (_A1, _A2) a i_begin
               ((i_begin, minus_nata i_end one_nata) :: lista)));;

let rec identify_blocks (_A1, _A2)
  a i = identify_blocks_main (_A1, _A2) a i [];;

let rec step_3_c_inner_loop _A
  vala l i k a =
    (if equal_nata k zero_nata then a
      else step_3_c_inner_loop _A vala (minus_nata l one_nata)
             (minus_nata i one_nata) (minus_nata k one_nata)
             (add_col_sub_row
               _A.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1
               vala i l a));;

let rec step_3_c _A
  x l k xa3 a = match x, l, k, xa3, a with x, l, k, [], a -> a
    | x, l, k, (i_begin, i_end) :: blocks, a ->
        (let aa =
           (if equal_nata i_end l then a
             else step_3_c_inner_loop _A
                    (divide
                      _A.division_ring_field.inverse_division_ring.divide_inverse
                      (index_mat a (i_end, k)) x)
                    l i_end (minus_nata (suc i_end) i_begin) a)
           in
          step_3_c _A x l k blocks aa);;

let rec step_3_a (_A1, _A2)
  i j a =
    (if equal_nata i zero_nata then a
      else (let aij = index_mat a (minus_nata i one_nata, j) in
            let aa =
              (if eq _A2
                    (index_mat a
                      (minus_nata i one_nata,
                        plus_nata (minus_nata i one_nata) one_nata))
                    (one _A1.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.numeral_neg_numeral.one_numeral) &&
                    not (eq _A2 aij
                          (zero _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero))
                then add_col_sub_row
                       _A1.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1
                       (uminus
                         _A1.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.group_add_neg_numeral.uminus_group_add
                         aij)
                       (suc (minus_nata i one_nata)) j a
                else a)
              in
             step_3_a (_A1, _A2) (minus_nata i one_nata) j aa));;

let rec step_3_main (_A1, _A2)
  n k a =
    (if less_eq_nat n k then a
      else (let b = step_3_a (_A1, _A2) (minus_nata k one_nata) k a in
            let all_blocks =
              identify_blocks
                (_A2, _A1.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1)
                b k
              in
            let blocks =
              filtera
                (fun block ->
                  not (eq _A2 (index_mat b (snd block, k))
                        (zero _A1.idom_divide_field.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero)))
                all_blocks
              in
            let aa =
              (if null blocks then b
                else (let (_, l) = find_largest_block (hda blocks) (tla blocks)
                        in
                      let x = index_mat b (l, k) in
                      let c = step_3_c _A1 x l k blocks b in
                      let d =
                        mult_col_div_row _A1
                          (inverse _A1.division_ring_field.inverse_division_ring
                            x)
                          k c
                        in
                      let e = swap_cols_rows_block (suc l) k d in
                       e))
              in
             step_3_main (_A1, _A2) n (suc k) aa));;

let rec step_3 (_A1, _A2) a = step_3_main (_A1, _A2) (dim_row a) one_nata a;;

let rec mpoly_to_poly (_A1, _A2, _A3, _A4, _A5)
  x p = Poly (let i =
                image ((ceq_poly_mapping
                         (ceq_nat, ccompare_nat, equal_nat, set_impl_nat)
                         (ceq_nat, ccompare_nat, zero_nat, equal_nat,
                           set_impl_nat)),
                        ccompare_poly_mapping)
                  (ceq_nat, ccompare_nat, set_impl_nat)
                  (fun m -> lookupa equal_nat zero_nat m x)
                  (filter
                    ((ceq_poly_mapping
                       (ceq_nat, ccompare_nat, equal_nat, set_impl_nat)
                       (ceq_nat, ccompare_nat, zero_nat, equal_nat,
                         set_impl_nat)),
                      ccompare_poly_mapping)
                    (fun m ->
                      ball (ceq_nat, ccompare_nat)
                        (keysb (ceq_nat, ccompare_nat, equal_nat, set_impl_nat)
                          (ceq_nat, ccompare_nat, zero_nat, equal_nat,
                            set_impl_nat)
                          m)
                        (fun y -> equal_nata y x))
                    (keysb
                      ((ceq_poly_mapping
                         (ceq_nat, ccompare_nat, equal_nat, set_impl_nat)
                         (ceq_nat, ccompare_nat, zero_nat, equal_nat,
                           set_impl_nat)),
                        ccompare_poly_mapping,
                        (equal_poly_mapping
                          (ceq_nat, ccompare_nat, equal_nat, set_impl_nat)
                          (ceq_nat, ccompare_nat, zero_nat, equal_nat,
                            set_impl_nat)),
                        set_impl_poly_mapping)
                      (_A1, _A2, _A3.monoid_add_comm_monoid_add.zero_monoid_add,
                        _A4, _A5)
                      (mapping_of _A3.monoid_add_comm_monoid_add.zero_monoid_add
                        p)))
                in
               (if set_eq (cenum_nat, ceq_nat, ccompare_nat) i
                     (set_empty (ceq_nat, ccompare_nat)
                       (of_phantom set_impl_nata))
                 then []
                 else map (fun n ->
                            coeff _A3.monoid_add_comm_monoid_add.zero_monoid_add
                              p (single (zero_nat, equal_nat) x n))
                        (upt zero_nata
                          (plus_nata
                            (maxa (ceq_nat, ccompare_nat, lattice_nat,
                                    linorder_nat)
                              i)
                            one_nata))));;

let rec polys_rf _A = comp (comp (map fst) snd) (yun_rf _A);;

let rec of_real_imag_ca
  x = (fun (real, imag) -> Complex (Real_of real, Real_of imag)) x;;

let rec initial_root_problem_complex
  p = (let n = degreea zero_complex p in
       let cs = coeffs zero_complex p in
       let rcs =
         remdups equal_complex
           (filtera
             (fun c -> not (is_rat_complexa c && is_int_rat (to_rat_complex c)))
             cs)
         in
       let pairs = map (fun c -> (c, min_int_poly_complex c)) rcs in
       let spairs =
         sort_key linorder_nat (fun (_, a) -> degreea zero_int a) pairs in
       let triples = zip (upt zero_nata (size_list spairs)) spairs in
       let mpoly =
         sum_list (monoid_add_mpoly (monoid_add_int, equal_int))
           (map (fun i ->
                  (let c = coeffa zero_complex p i in
                    times_mpolya (equal_int, semiring_0_int)
                      (monom (zero_int, equal_int)
                        (single (zero_nat, equal_nat) zero_nata i) one_inta)
                      (match
                        find (fun (_, (d, _)) -> equal_complexa d c) triples
                        with None ->
                          const (zero_int, equal_int) (to_int is_rat_complex c)
                        | Some (j, _) ->
                          var (one_int, zero_int, equal_int) (suc j))))
             (upt zero_nata (suc n)))
         in
        (mpoly, triples));;

let rec resultant_mpoly_poly (_A1, _A2, _A3, _A4, _A5)
  x p q =
    resultant_impl_basic
      ((equal_mpoly
         (_A1, _A2,
           _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
           _A3, _A5)),
        (idom_divide_mpoly (_A1, _A2, _A3, _A4, _A5)))
      (mpoly_to_mpoly_poly
        (_A1, _A2, _A3, _A4.idom_idom_divide.comm_ring_1_idom, _A5) x p)
      (map_poly
        _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero
        ((zero_mpoly
           _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero),
          (equal_mpoly
            (_A1, _A2,
              _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
              _A3, _A5)))
        (const
          (_A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero,
            _A3))
        q);;

let rec eliminate_aux_vars (_A1, _A2, _A3, _A4, _A5)
  p qs x2 = match p, qs, x2 with
    p, qs, [] ->
      mpoly_to_poly
        (_A1, _A2,
          _A4.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.comm_monoid_add_semiring_0,
          _A3, _A5)
        zero_nata p
    | p, qs, i :: is ->
        eliminate_aux_vars (_A1, _A2, _A3, _A4, _A5)
          (resultant_mpoly_poly (_A1, _A2, _A3, _A4, _A5) (suc i) p (qs i)) qs
          is;;

let rec representative_poly_complex
  p = (let (mp, triples) = initial_root_problem_complex p in
       let is = map fst triples in
       let qs = (fun j -> snd (snd (nth triples j))) in
        eliminate_aux_vars
          (ceq_int, ccompare_int, equal_int, idom_divide_int, set_impl_int) mp
          qs is);;

let rec roots_of_poly_dummy (_A1, _A2)
  p = failwith "roots-of-poly-dummy"
        (fun _ -> roots_of_poly_dummy (_A1, _A2) p);;

let rec roots_of_complex_rf_poly_part1
  xa = Abs_genuine_roots_aux
         (if list_all algebraic_complex
               (coeffs zero_complex (rep_rf_poly idom_complex xa))
           then (let q =
                   representative_poly_complex (rep_rf_poly idom_complex xa) in
                 let zeros = complex_roots_of_int_poly q in
                  (rep_rf_poly idom_complex xa,
                    (zeros,
                      (degreea zero_complex (rep_rf_poly idom_complex xa),
                        filter_fun_complex (rep_rf_poly idom_complex xa)))))
           else (rep_rf_poly idom_complex xa,
                  (roots_of_poly_dummy
                     (comm_ring_1_complex, ring_no_zero_divisors_complex)
                     (rep_rf_poly idom_complex xa),
                    (degreea zero_complex (rep_rf_poly idom_complex xa),
                      filter_fun_complex (rep_rf_poly idom_complex xa)))));;

let rec roots_of_complex_rf_poly
  p = genuine_roots_impl field_char_0_complex
        (roots_of_complex_rf_poly_part1 p);;

let rec roots_of_complex_rf_polys
  ps = maps roots_of_complex_rf_poly (polys_rf idom_complex ps);;

let rec yun_factorization (_A1, _A2, _A3)
  gcd p =
    (if is_zero
          _A1.factorial_ring_gcd_euclidean_ring_gcd.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
          p
      then (zero _A1.factorial_ring_gcd_euclidean_ring_gcd.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd,
             [])
      else (let c =
              coeffa
                _A1.factorial_ring_gcd_euclidean_ring_gcd.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
                p (degreea
                    _A1.factorial_ring_gcd_euclidean_ring_gcd.factorial_semiring_gcd_factorial_ring_gcd.semiring_Gcd_factorial_semiring_gcd.gcd_semiring_Gcd.gcd_Gcd.zero_gcd
                    p)
              in
            let q =
              smult (_A3, _A1.euclidean_ring_euclidean_ring_gcd.idom_modulo_euclidean_ring.idom_divide_idom_modulo.idom_idom_divide.semidom_idom.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.comm_semiring_0_comm_semiring_1,
                      _A1.euclidean_ring_euclidean_ring_gcd.idom_modulo_euclidean_ring.idom_divide_idom_modulo.idom_idom_divide.semidom_idom.semiring_1_no_zero_divisors_semidom.semiring_no_zero_divisors_semiring_1_no_zero_divisors)
                (inverse
                  _A2.field_field_char_0.division_ring_field.inverse_division_ring
                  c)
                p
              in
             (c, yun_monic_factorization
                   (_A1.factorial_ring_gcd_euclidean_ring_gcd, _A3) gcd q)));;

let rec yun_polys (_A1, _A2, _A3, _A4)
  xa = Abs_rf_polys
         (yun_factorization (_A1, _A2, _A4)
           (gcd_polyc (_A1.factorial_ring_gcd_euclidean_ring_gcd, _A3, _A4))
           xa);;

let rec roots_of_complex_poly
  p = (if equal_polya (zero_complex, equal_complex) p (zero_polya zero_complex)
        then []
        else roots_of_complex_rf_polys
               (yun_polys
                 (euclidean_ring_gcd_complex, field_char_0_complex,
                   semiring_gcd_mult_normalize_complex, equal_complex)
                 p));;

let rec complex_roots_of_complex_poly
  x = comp roots_of_complex_poly
        (poly_of_list (comm_monoid_add_complex, equal_complex)) x;;

let rec complex_roots_of_real_poly
  x = comp complex_roots_of_complex_poly
        (map (fun r -> of_real_imag_ca (r, zero_real_alga))) x;;

let rec lookup_other_ev _A
  ev i a =
    (if equal_nata i zero_nata then None
      else (if not (eq _A
                     (index_mat a
                       (minus_nata i one_nata, minus_nata i one_nata))
                     ev)
             then Some (minus_nata i one_nata)
             else lookup_other_ev _A ev (minus_nata i one_nata) a));;

let rec partition_ev_blocks _A
  a bs =
    (let n = dim_row a in
      (if equal_nata n zero_nata then bs
        else (match
               lookup_other_ev _A
                 (index_mat a (minus_nata n one_nata, minus_nata n one_nata))
                 (minus_nata n one_nata) a
               with None -> a :: bs
               | Some i ->
                 (let (ul, (_, (_, lr))) = split_block a (suc i) (suc i) in
                   partition_ev_blocks _A ul (lr :: bs)))));;

let rec jnf_vector_main (_A1, _A2)
  i_end a =
    (if equal_nata i_end zero_nata then []
      else (let i_start =
              identify_block (_A1, _A2) a (minus_nata i_end one_nata) in
             jnf_vector_main (_A1, _A2) i_start a @
               [(minus_nata (suc (minus_nata i_end one_nata)) i_start,
                  index_mat a (i_start, i_start))]));;

let rec jnf_vector (_A1, _A2) a = jnf_vector_main (_A1, _A2) (dim_row a) a;;

let rec triangular_to_jnf_vector (_A1, _A2)
  a = (let b = step_2 (_A1, _A2) (step_1 (_A1, _A2) a) in
        maps (comp (jnf_vector
                     (_A1.idom_divide_field.idom_idom_divide.comm_ring_1_idom.ring_1_comm_ring_1.neg_numeral_ring_1.numeral_neg_numeral.one_numeral,
                       _A2))
               (step_3 (_A1, _A2)))
          (partition_ev_blocks _A2 b []));;

let rec triangular_to_jnf_vector_ca
  x = triangular_to_jnf_vector (field_complex, equal_complex) x;;

let rec abs_ra x = abs_real_alga x;;

let one_ca : complex = one_complexa;;

let one_ra : real_alg = one_real_alga;;

let rec complex_roots_of_integer_poly
  x = comp (comp complex_roots_of_int_poly
             (poly_of_list (comm_monoid_add_int, equal_int)))
        (map (fun a -> Int_of_integer a)) x;;

let rec less_ra x = less_real_alg x;;

let rec plus_ca x = plus_complexa x;;

let rec plus_ra x = plus_real_alga x;;

let rec root_ra x = comp root_real_alg nat_of_integer x;;

let rec shows_prec_complex x = showsp_complex x;;

let rec show_ca
  x = comp implode (fun xa -> shows_prec_complex zero_nata xa []) x;;

let rec shows_prec_real_alg x = showsp_real_alg x;;

let rec show_ra
  x = comp implode (fun xa -> shows_prec_real_alg zero_nata xa []) x;;

let rec decompose_rat
  x = comp (map_prod integer_of_int integer_of_int) quotient_of x;;

let rec to_rational_ra x = comp decompose_rat to_rat_real_alg x;;

let rec sign_ra x = comp (comp fst to_rational_ra) sgn_real_alga x;;

let zero_ca : complex = zero_complexa;;

let zero_ra : real_alg = zero_real_alga;;

let rec complex_roots_of_rat_poly
  p = complex_roots_of_int_poly (snd (rat_to_int_poly p));;

let rec complex_roots_of_rational_poly
  x = comp (comp complex_roots_of_rat_poly
             (poly_of_list (comm_monoid_add_rat, equal_rat)))
        (map (fun (n, d) ->
               divide_rata (of_int (Int_of_integer n))
                 (of_int (Int_of_integer d))))
        x;;

let rec csqrt_ca x = csqrt x;;

let rec floor_ra x = comp integer_of_int floor_real_alg x;;

let rec minus_ca x = minus_complexa x;;

let rec minus_ra x = minus_real_alga x;;

let rec times_ca x = times_complexa x;;

let rec times_ra x = times_real_alga x;;

let rec divide_ca x = divide_complexa x;;

let rec divide_ra x = divide_real_alga x;;

let rec equals_ca x = equal_complexa x;;

let rec equals_ra x = equal_real_alg x;;

let rec uminus_ca x = uminus_complexa x;;

let rec uminus_ra x = uminus_real_alga x;;

let rec ceiling_ra x = comp integer_of_int (ceiling floor_ceiling_real_alg) x;;

let rec compare_ra x = compare_real_alg x;;

let rec imag_of_ca x = comp real_alg_of_real im x;;

let rec inverse_ca x = inverse_complexa x;;

let rec inverse_ra x = inverse_real_alga x;;

let rec maximum_ra x = max ord_real_alg x;;

let rec minimum_ra x = min ord_real_alg x;;

let rec real_of_ca x = comp real_alg_of_real re x;;

let rec decompose_ra
  x = comp (map_sum decompose_rat
             (map_prod (comp (map integer_of_int) (coeffs zero_int))
               integer_of_nat))
        info_real_alg x;;

let imag_unit_ca : complex = imaginary_unit;;

let rec less_equal_ra x = less_eq_real_alg x;;

let rec of_integer_ra
  x = comp (of_inta ring_1_real_alg) (fun a -> Int_of_integer a) x;;

let rec is_rational_ra x = is_rat_real_alg x;;

let rec of_rational_ca
  x = (fun (num, denom) ->
        of_rat field_char_0_complex
          (fract (Int_of_integer num) (Int_of_integer denom)))
        x;;

let rec of_rational_ra
  x = (fun (num, denom) ->
        of_rat_real_alg (fract (Int_of_integer num) (Int_of_integer denom)))
        x;;

end;; (*struct Algebraic*)
