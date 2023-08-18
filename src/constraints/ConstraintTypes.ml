open OurBase
(** Provides all modules related to constraints, i.e., atoms, constraints and formulas. *)

open PolyTypes

exception StrictUnremovable of string

(** A default atom module. *)
module type Atomizable = sig
  type t
  type value

  include Ring with type t := t

  val sub : t -> t -> t
  val of_constant : value -> t
  val of_var : Var.t -> t
  val vars : t -> VarSet.t
  val rename : RenameMap.t -> t -> t
  val is_linear : t -> bool
  val coeff_of_var : Var.t -> t -> value
  val get_constant : t -> value
  val of_coeff_list : value list -> Var.t list -> t
  val sum : t Sequence.t -> t
end

(** An atom is a comparison between two polynomials. *)
module type Atom = sig
  (** A comparator sets two values in a binary relation *)
  module Comparator : sig
    (** The different comparators.
              GT -> a > b.
              GE -> a >= b.
              LT -> a < b.
              LE -> a <= b. *)
    type t = GT  (** a > b *) | GE  (** a >= b *) | LT  (** a < b*) | LE  (** a <= b *)

    val values : t list
    (** Returns a list of all possible comparators. *)

    val str_values : string list
    (** Returns a list of all possible comparators with their string representation. *)

    val to_string : t -> string
    (** Returns string representing comparator. *)
  end

  type polynomial
  type value

  (* TODO Shouldn't be exposed *)
  module P : Atomizable with type t = polynomial and type value = value

  type t
  type compkind = LE | LT

  (** {1  {L Following methods are convenience methods for the creation of atoms.}} *)

  val mk : Comparator.t -> polynomial -> polynomial -> t
  (** Makes an atom out of two objects and a comparator. *)

  val mk_gt : polynomial -> polynomial -> t
  (** Makes an atom out of two objects using {b >} as the comparator. *)

  val mk_ge : polynomial -> polynomial -> t
  (** Makes an atom out of two objects using {b >=} as the comparator. *)

  val mk_lt : polynomial -> polynomial -> t
  (** Makes an atom out of two objects using {b <} as the comparator. *)

  val mk_le : polynomial -> polynomial -> t
  (** Makes an atom out of two objects using {b <=} as the comparator. *)

  module Infix : sig
    val ( > ) : polynomial -> polynomial -> t
    val ( >= ) : polynomial -> polynomial -> t
    val ( < ) : polynomial -> polynomial -> t
    val ( <= ) : polynomial -> polynomial -> t
  end

  val remove_strict : t -> t
  (** Express strict inequalities with non-strict counterparts. Only works iff all variables are discrete.
             Raises {i StrictUnremovable} otherwise *)

  (** {1  {L Following methods return certain properties of the atom. }}*)

  val ( =~= ) : t -> t -> bool

  val equal : t -> t -> bool
  (** Stable structural equality, but not an actual equality *)

  val compare : t -> t -> int
  (** Stable structural compare, but not an actual compare *)

  val neg : t -> t
  (** Returns the negation of an atom. *)

  val flip_comp : t -> t

  val to_string : ?to_file:bool -> ?pretty:bool -> t -> string
  (** Returns a string representing the atom. Parameter {i to_file} is used to get a representation with less special characters. *)

  val vars : t -> VarSet.t
  (** Returns the set of variables which are active in the atom.
            A variable is active, if it's value has an effect on the evaluation of the atom. *)

  val normalised_lhs : t -> polynomial
  (** Returns a normalised form of the atom, where the returned polynomial represents the atom in the form {i p <= 0}. *)

  (** {1  {L Following methods manipulate atoms and return the manipulated versions.}} *)

  val rename : t -> RenameMap.t -> t
  (** Assigns the variables of the atom new names based on the rename map. *)

  val fold : subject:(polynomial -> 'b) -> le:('b -> 'b -> 'c) -> lt:('b -> 'b -> 'c) -> t -> 'c
  (** Replaces all operations by new constructors. *)

  val is_linear : t -> bool
  (** Returns if both polynomials are linear. *)

  val get_coefficient : Var.t -> t -> value
  (** Returns the coefficient of a variable which is normalised to the lhs. *)

  val get_constant : t -> value
  (** Returns the single right hand side constant of the atom. *)

  val poly : t -> polynomial
  val is_lt : t -> bool
  val is_le : t -> bool
end

(** A constraint is a conjunction of atoms, i.e., we store those atoms in a list. *)
module type Constraint = sig
  type value
  type polynomial
  type compkind
  type atom
  type t

  (** {1  {L Following methods are convenience methods for the creation of constraints.}} *)

  val lift : atom -> t
  (** Lifts an atom to a constraint, i.e., a single atom {i a} is a constraint \[a\] as a constraint is a conjunction over atoms. *)

  val mk : atom list -> t
  (** Creates a constraint from a set of atoms. *)

  val mk_true : t
  (** Creates the trivial constraint {i true}, i.e., []. *)

  val mk_and : t -> t -> t
  (** Creates a new constraint as the conjunction of the atoms from two given constraints. *)

  val map_polynomial : (polynomial -> polynomial) -> t -> t
  (** TODO doc *)

  val mk_eq : polynomial -> polynomial -> t
  (** Creates a constraint that expresses the equality of the two polynomials. *)

  val mk_gt : polynomial -> polynomial -> t
  (** Creates a constraint that expresses for two polynomials {i p} and {i q} the comparison {i p > q}. *)

  val mk_ge : polynomial -> polynomial -> t
  (** Creates a constraint that expresses for two polynomials {i p} and {i q} the comparison {i p >= q}. *)

  val mk_lt : polynomial -> polynomial -> t
  (** Creates a constraint that expresses for two polynomials {i p} and {i q} the comparison {i p < q}. *)

  val mk_le : polynomial -> polynomial -> t
  (** Creates a constraint that expresses for two polynomials {i p} and {i q} the comparison {i p <= q}. *)

  val remove_strict : t -> t
  (** Express strict inequalities with non-strict counterparts. Only works iff all variables are discrete.
             Raises {i StrictUnremovable} otherwise *)

  module Infix : sig
    val ( = ) : polynomial -> polynomial -> t
    val ( > ) : polynomial -> polynomial -> t
    val ( >= ) : polynomial -> polynomial -> t
    val ( < ) : polynomial -> polynomial -> t
    val ( <= ) : polynomial -> polynomial -> t
    val ( && ) : t -> t -> t
  end

  val all : t list -> t
  (** Creates a constraint {i c1 && ... && cn}  of a list of constraints {i c1,...,cn}. *)

  val turn : t -> t
  (** Negates every atom of the constraint. *)

  (** {1  {L Following methods return certain properties of the constraint.}} *)

  val is_true : t -> bool
  (** Returns if the constraint is a tautology. We return true iff constraint is empty list \[\]. Thus, some cases [0 == 0] are missed? TODO doc? *)

  (* Returns if all polynomials contained in the constraint are linear *)
  val is_linear : t -> bool
  val ( =~= ) : t -> t -> bool

  val equal : t -> t -> bool
  (** Stable structural equality, but not an actual equality *)

  val compare : t -> t -> int
  (** Stable structural compare, but not an actual compare *)

  val vars : t -> VarSet.t
  (** Returns the set of variables which are active in the constraint.
            A variable is active, if it's value has an effect on the evaluation of the constraint. *)

  val to_string : ?to_file:bool -> ?pretty:bool -> ?conj:string -> t -> string
  (** Returns a string representing the constraint. Parameter {i to_file} is used to get a representation with less special characters. *)

  (** {1  {L Following methods manipulate constraints and return the manipulated versions.}} *)

  val rename : t -> RenameMap.t -> t
  (** Assigns the variables of the constraint new names based on the rename map *)

  val atom_list : t -> atom list
  (** Returns the corresponding list of atoms. *)

  val fold :
    subject:(polynomial -> 'b) ->
    le:('b -> 'b -> 'c) ->
    lt:('b -> 'b -> 'c) ->
    correct:'d ->
    conj:('d -> 'c -> 'd) ->
    t ->
    'd
  (** Replaces all operations by new constructors. *)

  val drop_nonlinear : t -> t
  (** Drops all nonlinear atoms from the constraints.
          Example: {i (a > 0 && b^2 < 2)} gets transformed to {i (a > 0)} *)

  val get_coefficient_vector : Var.t -> t -> value list
  (** Returns the row of all coefficients of a variable in a constraint, i.e., used for farkas quantor elimination. *)

  val get_matrix : Var.t list -> t -> value list list
  (** Returns the matrix of all coefficients of a variable from a set of variables in a constraint, i.e., used for farkas quantor elimination. *)

  val get_constant_vector : t -> value list
  (** Returns the row of all constants in a constraint, i.e., used for farkas quantor elimination.*)

  val dualise : Var.t list -> polynomial list list -> polynomial list -> t
  (** TODO doc *)
end

(** A formula is a propositional formula, a disjunction of constraints, i.e., a list of constraints or more precisely a list of atom lists. *)
module type Formula = sig
  type atom
  type constr
  type polynomial
  type value
  type t

  (** {1  {L Following methods are convenience methods for the creation of formulas.}} *)

  val mk : constr -> t
  (** Creates a formula from a set of constraints. *)

  val lift : constr list -> t

  val mk_true : t
  (** Creates the trivial formula {i true}, i.e., [true]. *)

  val mk_false : t
  (** Creates the trivial formula {i false}, i.e., [false]. *)

  val mk_and : t -> t -> t
  (** Returns the (normalized) conjunction of two formulas. *)

  val mk_or : t -> t -> t
  (** Returns the disjunction of two formulas. *)

  val neg : t -> t
  (** Returns the negated formula. *)

  val implies : t -> t -> t
  (** Returns for two formulas {i f,g} the the formula {i f -> g} *)

  val constraints : t -> constr list
  (** Returns a list of all constraints occuring in the disjunction. *)

  val map_polynomial : (polynomial -> polynomial) -> t -> t
  (** TODO doc *)

  val mk_eq : polynomial -> polynomial -> t
  (** Creates a formula that expresses the equality of the two polynomials. *)

  val mk_gt : polynomial -> polynomial -> t
  (** Creates a formula that expresses for two polynomials {i p} and {i q} the comparison {i p > q}. *)

  val mk_ge : polynomial -> polynomial -> t
  (** Creates a formula that expresses for two polynomials {i p} and {i q} the comparison {i p >= q}. *)

  val mk_lt : polynomial -> polynomial -> t
  (** Creates a formula that expresses for two polynomials {i p} and {i q} the comparison {i p < q}. *)

  val mk_le : polynomial -> polynomial -> t
  (** Creates a formula that expresses for two polynomials {i p} and {i q} the comparison {i p <= q}. *)

  val mk_uneq : polynomial -> polynomial -> t
  (** Creates a formula that expresses the unequality of the two polynomials. *)

  val le_than_any : polynomial -> polynomial list -> t
  (** Returns the formula {i (p <= p1 || ... || p <= pn) } for a polynomial {i p} and a set of polynomials {i p1,...,pn}.*)

  val le_than_all : polynomial -> polynomial list -> t
  (** Returns the formula {i (p <= p1 && ... && p <= pn) } for a polynomial {i p} and a set of polynomials {i p1,...,pn}.*)

  val remove_strict : t -> t
  (** Express strict inequalities with non-strict counterparts. Only works iff all variables are discrete.
             Raises {i StrictUnremovable} otherwise *)

  val all : t list -> t
  (** Creates a formula {i (f1 && ... && fn)}  of a list of constraints {i f1,...,fn}. *)

  val any : t list -> t
  (** Creates a formula {i (f1 || ... || fn)}  of a list of constraints {i f1,...,fn}. *)

  val turn : t -> t
  (** Negates every atom of every constraint of the formula. *)

  (* Checks whether all polynomials occuring in this formula are linear *)
  val is_linear : t -> bool
  val is_true : t -> bool
  val atoms : t -> atom list

  module Infix : sig
    val ( = ) : polynomial -> polynomial -> t
    val ( > ) : polynomial -> polynomial -> t
    val ( >= ) : polynomial -> polynomial -> t
    val ( < ) : polynomial -> polynomial -> t
    val ( <= ) : polynomial -> polynomial -> t
    val ( && ) : t -> t -> t
    val ( || ) : t -> t -> t
    val ( => ) : t -> t -> t
    val ( <=> ) : t -> t -> t
  end

  (** {1  {L Following methods return certain properties of the formula.}} *)

  val vars : t -> VarSet.t
  (** Returns the set of variables which are active in the formula.
            A variable is active, if it's value has an effect on the evaluation of the constraint. *)

  val to_string : ?pretty:bool -> t -> string
  (** Returns a string representing the constraint. *)

  val to_string_formatted : t -> FormattedString.t

  (** {1  {L  Following methods manipulate atoms and return the manipulated versions. }}*)

  val rename : t -> RenameMap.t -> t
  (** Assigns the variables of the constraint new names based on the rename map *)

  val fold :
    subject:(polynomial -> 'b) ->
    le:('b -> 'b -> 'c) ->
    lt:('b -> 'b -> 'c) ->
    correct:'d ->
    conj:('d -> 'c -> 'd) ->
    wrong:'e ->
    disj:('e -> 'd -> 'e) ->
    t ->
    'e
  (** Replaces all operations by new constructors. *)
end
