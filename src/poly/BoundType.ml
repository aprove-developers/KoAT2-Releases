(** Implementation of bounds, i.e., polynomials, exponential terms and max/min terms.*)
open Batteries
open Polynomials

(** A MinMaxPolynomial is a polynomial which allows the usage of min and max functions  *)
module type Bound =
  sig
    type t
    type value
    type polynomial

    (* A finite bound *)
    type bound

    (* Obtain a finite bound whenever it is possible *)
    val prove_finiteness: t -> bound option

    include PolyTypes.Evaluable with type t := t with type value := value
    (* include PolyTypes.Math with type t := t *)
    include PolyTypes.PartialOrder with type t := t

    (** {1  {L Following methods are convenience methods for the creation of bounds.}} *)

    (** Creates a bound from a polynomial *)
    val of_poly : polynomial -> t

    (** Tries to convert the bound to a polynomial if possible *)
    val to_poly : t -> polynomial Option.t

    (** Creates a constant bound from a constant value. *)
    val of_constant : value -> t

    (** Is the bound constant? *)
    val is_constant : t -> bool

    (** Creates a constant bound from an integer. *)
    val of_int : int -> t

    (** Transforms a bound into an integer. *)
    val to_int : t -> int

    (** Creates a bound from a Variable. *)
    val of_var : Var.t -> t

    (** Creates a bound of a string representing a variable. *)
    val of_var_string : string -> t

    (** Returns the infinity bound. *)
    val infinity : t

    (** Returns for a positive integer value i and a bound b the new bound b^i. *)
    val exp : value -> t -> t

    (** Returns for a polynomial bound the maximal occuring constant. TODO doc *)
    val max_of_occurring_constants : t -> value

    (** Returns true iff. a bound is infinite. *)
    val is_infinity : t -> bool

    (** Returns true iff. a bound is finite. *)
    val is_finite : t -> bool

    (** Creates a string representing the bound by calling {b show} with complexity enabled. *)
    val to_string : ?pretty:bool -> ?termination_only:bool -> t -> string

    (** Creates a string that represents the finiteness of the bound*)
    val show_finiteness : t -> string

    (** Generates a string from a bound and adds the asymptotic complexity if parameter {i complexity} is not assigned to false. *)
    val show : ?pretty:bool -> ?complexity:bool -> ?termination_only:bool -> t -> string

    (** Math functions. Since we can not negate or subtract bounds, these functions form a prober subset of PolyTypes.Math *)
    (** Returns zero element. *)
    val zero : t

    (** Returns one element. *)
    val one : t

    (** Returns sum of two element. *)
    val add : t -> t -> t

    (** Returns product of two element. *)
    val mul : t -> t -> t

    (** Returns element to the power of some provided integer value. *)
    val pow : t -> int -> t

    (** Returns the sum of all enums elements. *)
    val sum : t Enum.t -> t

    (** Returns the sum of all sequence elements. *)
    val sum_sequence : t Base.Sequence.t -> t

    (** Returns the sum of all list elements. *)
    val sum_list : t list -> t

    (** Returns the product of all enums elements. *)
    val product : t Enum.t -> t

    (** Returns the product of all enums elements. *)
    val product_sequence : t Base.Sequence.t -> t

    (** Addition of two elements. *)
    val (+) : t -> t -> t

    (** Multiplication of two elements *)
    val ( * ) : t -> t -> t

    (** Raises an element to the power of an integer value. *)
    val ( ** ) : t -> int -> t

    (** Functions to classify the quality of the bound *)


    (** {1  {L Following methods can be used to classify the type of the bound. }}*)

    (** Substitutes every occurrence of the variable in the polynomial by the replacement polynomial.
            Ignores naming equalities. *)
    val substitute : Var.t -> replacement:t -> t -> t

    (** Substitutes every occurrence of the variables in the polynomial by the corresponding replacement polynomial.
            Leaves all variables unchanged which are not in the replacement map.  *)
    val substitute_all : t Map.Make(Var).t -> t -> t

    (** Substitutes every occurrence of the variables in the polynomial by the corresponding replacement polynomial. *)
    val substitute_f : (Var.t -> t) -> t -> t

    (** Replaces all arithmetical operations by new constructors. *)
    val fold : const:(value -> 'b) ->
            var:(Var.t -> 'b) ->
            plus:('b -> 'b -> 'b) ->
            times:('b -> 'b -> 'b) ->
            exp:(value -> 'b -> 'b) ->
            inf:'b ->
            t -> 'b

    (** Replaces all arithmetical operations by new constructors in finite bounds. *)
    val fold_bound : const:(value -> 'b) ->
            var:(Var.t -> 'b) ->
            plus:('b -> 'b -> 'b) ->
            times:('b -> 'b -> 'b) ->
            exp:(value -> 'b -> 'b) ->
            bound -> 'b

    (** TODO doc *)
    type complexity =
    Inf
    (** Bound is infinite. *)
    | Polynomial of int
    (** Bound is in asymptotic class O(n^i) *)
    | Exponential of int
    (** Bound is in corresponding asymptotic class O(2^2^...^n) where the integer value denotes the amount of powers.*)

    (** TODO doc where is this method? Returns true iff. two bounds are equal. Or asym. equal?*)
    val equal_complexity : complexity -> complexity -> bool

    (** Returns string representing asymptotic complexity class. *)
    val show_complexity : complexity -> string

    (** Returns string representing asymptotic complexity class in the TermComp format. *)
    val show_complexity_termcomp : complexity -> string

    (** Returns an overapproximation of the asymptotic complexity of the given bound. *)
    val asymptotic_complexity : t -> complexity

    (** Returns true iff the asymptotic complexity is n^1. *)
    val is_linear : t -> bool

    (** Returns true iff the asymptotic complexity is polynomial. *)
    val is_polynomial : t -> bool

    (** -1 if first bound asy. lower, 0 if both are asym. equal, 1 otherwise *)
    val compare_asy : t -> t -> int

    (** Returns the asy. smaller bound (if both are equal than the first argument)*)
    val min_asy : t -> t -> t

    (** Needed for Atomizable but not yet implemented. *)
    val coeff_of_var : Var.t -> t -> value

    (** Needed for Atomizable but not yet implemented. *)
    val of_coeff_list : value list -> Var.t list -> t

    (** Returns the constant of a bound *)
    val get_constant : t -> value

    (* Uses a heuristic to keep the 'better' of both bounds.
    * It first compares the asymptotic complexity,
    * then the number of occuring variables,
    * and finally the syntactic complexity
    * of both bounds. *)
    val keep_simpler_bound : t -> t -> t
end
