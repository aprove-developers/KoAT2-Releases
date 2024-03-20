open! OurBase
(** Implementation of bounds, i.e., polynomials, exponential, and logarithmic terms.*)

(** A MinMaxPolynomial is a polynomial which allows the usage of min and max functions  *)
module type Bound = sig
  type t
  type value
  type polynomial

  (* A finite bound *)
  type bound

  (* Obtain a finite bound whenever it is possible *)
  val prove_finiteness : t -> bound option

  include PolyTypes.Evaluable with type t := t with type value := value

  (* include PolyTypes.Math with type t := t *)
  include PolyTypes.PartialOrder with type t := t

  (** {1  {L Following methods are convenience methods for the creation of bounds.}} *)

  val of_poly : polynomial -> t
  (** Creates a bound from a polynomial *)

  val of_intpoly : Polynomials.Polynomial.t -> t
  (** Creates a bound from a polynomial *)

  val to_poly : t -> polynomial Option.t
  (** Tries to convert the bound to a polynomial if possible *)

  val of_constant : value -> t
  (** Creates a constant bound from a constant value. *)

  val of_OurInt : OurInt.t -> t
  (** Creates a constant bound from a constant value. *)

  val is_constant : t -> bool
  (** Is the bound constant? *)

  val of_int : int -> t
  (** Creates a constant bound from an integer. *)

  val of_var : Var.t -> t
  (** Creates a bound from a Variable. *)

  val of_var_string : string -> t
  (** Creates a bound of a string representing a variable. *)

  val infinity : t
  (** Returns the infinity bound. *)

  val exp : value -> t -> t
  (** Returns for a value i and a bound b the new bound i^b. *)

  val exp_int : OurInt.t -> t -> t
  (** Returns for a positive integer value i and a bound b the new bound b^i. *)

  val log : Var.t -> t
  val log_of_constant : value -> t
  val log_of_poly : Polynomials.Polynomial.t -> t
  val log_of_bound : t -> t

  val max_of_occurring_constants : t -> value
  (** Returns for a polynomial bound the maximal occuring constant. TODO doc *)

  val is_infinity : t -> bool
  (** Returns true iff. a bound is infinite. *)

  val is_finite : t -> bool
  (** Returns true iff. a bound is finite. *)

  val to_string : ?pretty:bool -> ?termination_only:bool -> t -> string
  (** Creates a string representing the bound by calling {b show} with complexity enabled. *)

  val show_finiteness : t -> string
  (** Creates a string that represents the finiteness of the bound*)

  val show : ?pretty:bool -> ?complexity:bool -> ?termination_only:bool -> t -> string
  (** Generates a string from a bound and adds the asymptotic complexity if parameter {i complexity} is not assigned to false. *)

  val zero : t
  (** Math functions. Since we can not negate or subtract bounds, these functions form a prober subset of PolyTypes.Math
        Returns zero element. *)

  val one : t
  (** Returns one element. *)

  val add : t -> t -> t
  (** Returns sum of two element. *)

  val mul : t -> t -> t
  (** Returns product of two element. *)

  val pow : t -> int -> t
  (** Returns element to the power of some provided integer value. *)

  val sum : t Sequence.t -> t
  (** Returns the sum of all sequence elements. *)

  val sum_list : t list -> t
  (** Returns the sum of all list elements. *)

  val product : t Sequence.t -> t
  (** Returns the product of all sequence elements. *)

  val ( + ) : t -> t -> t
  (** Addition of two elements. *)

  val ( * ) : t -> t -> t
  (** Multiplication of two elements *)

  val ( ** ) : t -> int -> t
  (** Raises an element to the power of an integer value. *)

  (** Functions to classify the quality of the bound *)

  (** {1  {L Following methods can be used to classify the type of the bound. }}*)

  val substitute : Var.t -> replacement:t -> t -> t
  (** Substitutes every occurrence of the variable in the polynomial by the replacement polynomial.
            Ignores naming equalities. *)

  val substitute_all : (Var.t, t, Var.comparator_witness) Map.t -> t -> t
  (** Substitutes every occurrence of the variables in the polynomial by the corresponding replacement polynomial.
            Leaves all variables unchanged which are not in the replacement map.  *)

  val substitute_f : (Var.t -> t) -> t -> t
  (** Substitutes every occurrence of the variables in the polynomial by the corresponding replacement polynomial. *)

  type complexity
  (** TODO doc *)

  val equal_complexity : complexity -> complexity -> bool
  (** TODO doc where is this method? Returns true iff. two bounds are equal. Or asym. equal?*)

  val show_complexity : complexity -> string
  (** Returns string representing asymptotic complexity class. *)

  val show_complexity_termcomp : complexity -> string
  (** Returns string representing asymptotic complexity class in the TermComp format. *)

  val asymptotic_complexity : t -> complexity
  (** Returns an overapproximation of the asymptotic complexity of the given bound. *)

  val is_linear : t -> bool
  (** Returns true iff the asymptotic complexity is n^1. *)

  val is_polynomial : t -> bool
  (** Returns true iff the asymptotic complexity is polynomial. *)

  val compare_asy : t -> t -> int
  (** -1 if first bound asy. lower, 0 if both are asym. equal, 1 otherwise *)

  val min_asy : t -> t -> t
  (** Returns the asy. smaller bound (if both are equal than the first argument)*)

  val coeff_of_var : Var.t -> t -> value
  (** Needed for Atomizable but not yet implemented. *)

  val of_coeff_list : value list -> Var.t list -> t
  (** Needed for Atomizable but not yet implemented. *)

  (* Uses a heuristic to keep the 'better' of both bounds.
     * It first compares the asymptotic complexity,
     * then the number of occuring variables,
     * and finally the syntactic complexity
     * of both bounds. *)
  val keep_simpler_bound : t -> t -> t
end
