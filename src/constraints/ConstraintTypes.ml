(** Provides all modules related to constraints, i.e., atoms, constraints and formulas. *)
open Batteries
open PolyTypes

 (** A default atom module. *)
module type Atomizable =
  sig
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
  end
   
(** An atom is a comparison between two polynomials. *)
module type Atom =
  sig
    (** A comparator sets two values in a binary relation *)
        module Comparator : sig

          (** The different comparators.
              GT -> a > b.
              GE -> a >= b.
              LT -> a < b.
              LE -> a <= b. *)
          type t = GT 
          (** a > b *) 
          | GE
          (** a >= b *) 
          | LT 
          (** a < b*)
          | LE
          (** a <= b *)

          (** Returns a list of all possible comparators. *)
          val values : t list

          (** Returns a list of all possible comparators with their string representation. *)
          val str_values : string list

          (** Returns string representing comparator. *)
          val to_string : t -> string

        end
                                                  
        type polynomial
        type value

        (* TODO Shouldn't be exposed *)
        module P : Atomizable
               with type t = polynomial
                and type value = value

        type t
             

        (** {1  {L Following methods are convenience methods for the creation of atoms.}} *)
          
        (** Makes an atom out of two objects and a comparator. *)
        val mk : Comparator.t -> polynomial -> polynomial -> t

        (** Makes an atom out of two objects using {b >} as the comparator. *)
        val mk_gt : polynomial -> polynomial -> t

        (** Makes an atom out of two objects using {b >=} as the comparator. *)
        val mk_ge : polynomial -> polynomial -> t

        (** Makes an atom out of two objects using {b <} as the comparator. *)
        val mk_lt : polynomial -> polynomial -> t

        (** Makes an atom out of two objects using {b <=} as the comparator. *)
        val mk_le : polynomial -> polynomial -> t

        module Infix : sig
          val (>) : polynomial -> polynomial -> t
          val (>=) : polynomial -> polynomial -> t
          val (<) : polynomial -> polynomial -> t
          val (<=) : polynomial -> polynomial -> t
        end

        
        (** {1  {L Following methods return certain properties of the atom. }}*)
          
        val (=~=) : t -> t -> bool

        (** Stable structural equality, but not an actual equality *)
        val equal : t -> t -> bool
          
        (** Stable structural compare, but not an actual compare *)
        val compare : t -> t -> int
          
         (** Returns the negation of an atom. *)
        val neg : t -> t
          
        (** Returns a string representing the atom. Parameter {i to_file} is used to get a representation with less special characters. *)
        val to_string : ?to_file:bool -> ?comp:string -> t -> string

        (** Returns the set of variables which are active in the atom.
            A variable is active, if it's value has an effect on the evaluation of the atom. *)
        val vars : t -> VarSet.t

        (** Returns a normalised form of the atom, where the returned polynomial represents the atom in the form {i p <= 0}. *)
        val normalised_lhs : t -> polynomial

        (** {1  {L Following methods manipulate atoms and return the manipulated versions.}} *)

        (** Assigns the variables of the atom new names based on the rename map. *)
        val rename : t -> RenameMap.t -> t

        (** Replaces all operations by new constructors. *)
        val fold : subject:(polynomial -> 'b) ->
                   le:('b -> 'b -> 'c) ->
                   t -> 'c
                   
        (** Returns if both polynomials are linear. *)
        val is_linear : t -> bool
    
        (** Returns the coefficient of a variable which is normalised to the lhs. *)
        val get_coefficient : Var.t -> t -> value
          
        (** Returns the single right hand side constant of the atom. *)
        val get_constant : t -> value

  end

(** A constraint is a conjunction of atoms, i.e., we store those atoms in a list. *)
module type Constraint =
  sig
        type value
        type polynomial
        type atom
        type t
        
        (* TODO Shouldn't be exposed *)
        module A : Atom
               with type t = atom
                and type polynomial = polynomial
                and type value = value

        (** {1  {L Following methods are convenience methods for the creation of constraints.}} *)
          
        (** Lifts an atom to a constraint, i.e., a single atom {i a} is a constraint \[a\] as a constraint is a conjunction over atoms. *)
        val lift : atom -> t      

        (** Creates a constraint from a set of atoms. *)
        val mk : atom list -> t

        (** Creates the trivial constraint {i true}, i.e., []. *)
        val mk_true : t

        (** Creates a new constraint as the conjunction of the atoms from two given constraints. *)
        val mk_and : t -> t -> t

        (** TODO doc *)
        val map_polynomial : (polynomial -> polynomial) -> t -> t

        (** Creates a constraint that expresses the equality of the two polynomials. *)
        val mk_eq : polynomial -> polynomial -> t

        (** Creates a constraint that expresses for two polynomials {i p} and {i q} the comparision {i p > q}. *)
        val mk_gt : polynomial -> polynomial -> t

        (** Creates a constraint that expresses for two polynomials {i p} and {i q} the comparision {i p >= q}. *)
        val mk_ge : polynomial -> polynomial -> t

        (** Creates a constraint that expresses for two polynomials {i p} and {i q} the comparision {i p < q}. *)
        val mk_lt : polynomial -> polynomial -> t

        (** Creates a constraint that expresses for two polynomials {i p} and {i q} the comparision {i p <= q}. *)
        val mk_le : polynomial -> polynomial -> t

        module Infix : sig
          val (=) : polynomial -> polynomial -> t
          val (>) : polynomial -> polynomial -> t
          val (>=) : polynomial -> polynomial -> t
          val (<) : polynomial -> polynomial -> t
          val (<=) : polynomial -> polynomial -> t
          val (&&) : t -> t -> t
        end

        (** Creates a constraint {i c1 && ... && cn}  of a list of constraints {i c1,...,cn}. *)
        val all : t list -> t
          
        (** Negates every atom of the constraint. *)
        val turn : t -> t

        (** {1  {L Following methods return certain properties of the constraint.}} *)
          
        (** Returns if the constraint is a tautology. We return true iff constraint is empty list \[\]. Thus, some cases [0 == 0] are missed? TODO doc? *)
        val is_true : t -> bool
          
        val (=~=) : t -> t -> bool
        
        (** Stable structural equality, but not an actual equality *)
        val equal : t -> t -> bool

        (** Stable structural compare, but not an actual compare *)
        val compare : t -> t -> int

        (** Returns the set of variables which are active in the constraint.
            A variable is active, if it's value has an effect on the evaluation of the constraint. *)
        val vars : t -> VarSet.t

        (** Returns a string representing the constraint. Parameter {i to_file} is used to get a representation with less special characters. *)
        val to_string : ?to_file:bool -> ?comp:string -> ?conj:string -> t -> string


        (** {1  {L Following methods manipulate constraints and return the manipulated versions.}} *)

        (** Assigns the variables of the constraint new names based on the rename map *)
        val rename : t -> RenameMap.t -> t

        (** Returns the corresponding list of atoms. *)
        val atom_list : t -> atom list
          
        (** Replaces all operations by new constructors. *)
        val fold : subject:(polynomial -> 'b) ->
                   le:('b -> 'b -> 'c) ->
                   correct:('d) ->
                   conj:('d -> 'c -> 'd) ->
                   t -> 'd
                   
        (** Drops all nonlinear atoms from the constraints. 
          Example: {i (a > 0 && b^2 < 2)} gets transformed to {i (a > 0)} *)
        val drop_nonlinear : t -> t
        
        (** Returns the row of all coefficients of a variable in a constraint, i.e., used for farkas quantor elimination. *)
        val get_coefficient_vector : Var.t -> t -> value list
          
        (** Returns the matrix of all coefficients of a variable from a set of variables in a constraint, i.e., used for farkas quantor elimination. *)
        val get_matrix : VarSet.t -> t -> value list list
          
        (** Returns the row of all constants in a constraint, i.e., used for farkas quantor elimination.*)
        val get_constant_vector : t -> value list
          
        (** TODO doc *)
        val dualise : Var.t list -> value list list -> polynomial list -> t
          
  end

(** A formula is a propositional formula, a disjunction of constraints, i.e., a list of constraints or more precisely a list of atom lists. *)
module type Formula =
  sig
        type atom
        type constr
        type polynomial
        type value
       
        type t
        
        (** {1  {L Following methods are convenience methods for the creation of formulas.}} *)
        
        (** Lifts an atom to a formula, i.e., a single atom {i a} is a formula \[\[a\]\] as a formula is a disjunction over constraints. *)
        val lift : atom -> t 
        
        (** Creates a formula from a set of constraints. *)     
        val mk : constr -> t      

        (** Creates the trivial formula {i true}, i.e., [true]. *)
        val mk_true : t

        (** Returns the (normalized) conjunction of two formulas. *)
        val mk_and : t -> t -> t

        (** Returns the disjunction of two formulas. *)
        val mk_or : t -> t -> t
        
        (** Returns the negated formula. *)
        val neg : t -> t

        (** Returns for two formulas {i f,g} the the formula {i f -> g} *)
        val implies : t -> t -> t

        (** Returns a list of all constraints occuring in the disjunction. *)
        val constraints : t -> constr list

        (** TODO doc *)
        val map_polynomial : (polynomial -> polynomial) -> t -> t
          
        (** Creates a formula that expresses the equality of the two polynomials. *)
        val mk_eq : polynomial -> polynomial -> t

        (** Creates a formula that expresses for two polynomials {i p} and {i q} the comparision {i p > q}. *)
        val mk_gt : polynomial -> polynomial -> t

        (** Creates a formula that expresses for two polynomials {i p} and {i q} the comparision {i p >= q}. *)
        val mk_ge : polynomial -> polynomial -> t

        (** Creates a formula that expresses for two polynomials {i p} and {i q} the comparision {i p < q}. *)
        val mk_lt : polynomial -> polynomial -> t

        (** Creates a formula that expresses for two polynomials {i p} and {i q} the comparision {i p <= q}. *)
        val mk_le : polynomial -> polynomial -> t

        (** Creates a formula that expresses the unequality of the two polynomials. *)
        val mk_uneq : polynomial -> polynomial -> t

        (** Returns the formula {i (p <= p1 || ... || p <= pn) } for a polynomial {i p} and a set of polynomials {i p1,...,pn}.*)
        val le_than_any : polynomial -> polynomial list -> t

        (** Returns the formula {i (p <= p1 && ... && p <= pn) } for a polynomial {i p} and a set of polynomials {i p1,...,pn}.*)
        val le_than_all : polynomial -> polynomial list -> t
          
        (** Creates a formula {i (f1 && ... && fn)}  of a list of constraints {i f1,...,fn}. *)
        val all : t list -> t

        (** Creates a formula {i (f1 || ... || fn)}  of a list of constraints {i f1,...,fn}. *)
        val any : t list -> t

        (** Negates every atom of every constraint of the formula. *)
        val turn : t -> t
          
        module Infix : sig
          val (=) : polynomial -> polynomial -> t
          val (>) : polynomial -> polynomial -> t
          val (>=) : polynomial -> polynomial -> t
          val (<) : polynomial -> polynomial -> t
          val (<=) : polynomial -> polynomial -> t
          val (&&) : t -> t -> t
          val (||) : t -> t -> t
          val (=>) : t -> t -> t
          val (<=>) : t -> t -> t
        end

        (** {1  {L Following methods return certain properties of the formula.}} *)
          
        (** Returns the set of variables which are active in the formula.
            A variable is active, if it's value has an effect on the evaluation of the constraint. *)
        val vars : t -> VarSet.t

        (** Returns a string representing the constraint. *)
        val to_string : t -> string


        (** {1  {L  Following methods manipulate atoms and return the manipulated versions. }}*)

        (** Assigns the variables of the constraint new names based on the rename map *)
        val rename : t -> RenameMap.t -> t

        (** Replaces all operations by new constructors. *)
        val fold : subject:(polynomial -> 'b) ->
                   le:('b -> 'b -> 'c) ->
                   correct:('d) ->
                   conj:('d -> 'c -> 'd) ->
                   wrong:('e) ->
                   disj:('e -> 'd -> 'e) ->
                   t -> 'e
end