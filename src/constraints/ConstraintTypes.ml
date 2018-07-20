open Batteries
open PolyTypes

(** Provides all module types related to constraints *)

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
   
(** An atom is a comparison between two polynomials *)
module type Atom =
  sig
    (** A comparator sets two values in a binary relation *)
        module Comparator : sig

          (** The different comparators.
              GT -> a > b.
              GE -> a >= b.
              LT -> a < b.
              LE -> a <= b. *)
          type t = GT | GE | LT | LE

          (** Returns a list of all possible comparators *)
          val values : t list

          (** Returns a list of all possible comparators with their string representation *)
          val str_values : string list

          val to_string : t -> string

        end
                                                  
        type polynomial
        type value

        (* TODO Shouldn't be exposed *)
        module P : Atomizable
               with type t = polynomial
                and type value = value

        type t
             

        (** Following methods are convenience methods for the creation of atoms. *)
          
        val mk : Comparator.t -> polynomial -> polynomial -> t
        val mk_gt : polynomial -> polynomial -> t
        val mk_ge : polynomial -> polynomial -> t
        val mk_lt : polynomial -> polynomial -> t
        val mk_le : polynomial -> polynomial -> t

        module Infix : sig
          val (>) : polynomial -> polynomial -> t
          val (>=) : polynomial -> polynomial -> t
          val (<) : polynomial -> polynomial -> t
          val (<=) : polynomial -> polynomial -> t
        end

        (** Following methods return certain properties of the atom. *)
          
        val (=~=) : t -> t -> bool

        (** Stable structural equality, but not an actual equality *)
        val equal : t -> t -> bool
          
        (** Stable structural compare, but not an actual compare *)
        val compare : t -> t -> int
          
        val neg : t -> t
          
        val to_string : ?comp:string -> t -> string

        (** Returns the set of variables which are active in the atom.
            A variable is active, if it's value has an effect on the evaluation of the atom. *)
        val vars : t -> VarSet.t

        (** Returns a normalised form of the atom, where the returned polynomial represents the atom in the form p <= 0. *)
        val normalised_lhs : t -> polynomial

        (** Following methods manipulate atoms and return the manipulated versions. *)

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

(** A constraint is a conjunction of atoms *)
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

        (** Following methods are convenience methods for the creation of atoms. *)
          
        val lift : atom -> t      
        val mk : atom list -> t
        val mk_true : t
        val mk_and : t -> t -> t

        val map_polynomial : (polynomial -> polynomial) -> t -> t

        (** Creates a constraint that expresses the equality of the two polynomials. *)
        val mk_eq : polynomial -> polynomial -> t

        val mk_gt : polynomial -> polynomial -> t
        val mk_ge : polynomial -> polynomial -> t
        val mk_lt : polynomial -> polynomial -> t
        val mk_le : polynomial -> polynomial -> t

        module Infix : sig
          val (=) : polynomial -> polynomial -> t
          val (>) : polynomial -> polynomial -> t
          val (>=) : polynomial -> polynomial -> t
          val (<) : polynomial -> polynomial -> t
          val (<=) : polynomial -> polynomial -> t
          val (&&) : t -> t -> t
        end

        val all : t list -> t
          
        val turn : t -> t

        (** Following methods return certain properties of the constraint. *)
          
        (** Returns if the constraint is a tautology *)
        val is_true : t -> bool
          
        val (=~=) : t -> t -> bool
        
        (** Stable structural equality, but not an actual equality *)
        val equal : t -> t -> bool

        (** Stable structural compare, but not an actual compare *)
        val compare : t -> t -> int

        (** Returns the set of variables which are active in the constraint.
            A variable is active, if it's value has an effect on the evaluation of the constraint. *)
        val vars : t -> VarSet.t

        val to_string : ?comp:string -> ?conj:string -> t -> string


        (** Following methods manipulate atoms and return the manipulated versions. *)

        (** Assigns the variables of the constraint new names based on the rename map *)
        val rename : t -> RenameMap.t -> t

        val atom_list : t -> atom list
          
        (** Replaces all operations by new constructors. *)
        val fold : subject:(polynomial -> 'b) ->
                   le:('b -> 'b -> 'c) ->
                   correct:('d) ->
                   conj:('d -> 'c -> 'd) ->
                   t -> 'd
                   
        (** Drops all nonlinear atoms from the constraints. 
          Example: (a > 0 && b^2 < 2) gets transformed to (a > 0) *)
        val drop_nonlinear : t -> t
        
          (** Returns the row of all coefficients of a variable in a constraint...used for farkas quantor elimination*)
        val get_coefficient_vector : Var.t -> t -> value list
          
        val get_matrix : VarSet.t -> t -> value list list
          
        (** Returns the row of all coefficients of a variable in a constraint...used for farkas quantor elimination*)
        val get_constant_vector : t -> value list
          
        val dualise : Var.t list -> value list list -> polynomial list -> t
          
  end

(** A formula is a propositional formula *)
module type Formula =
  sig
        type atom
        type constr
        type polynomial
        type value
       
        type t
        

        (** Following methods are convenience methods for the creation of atoms. *)
          
        val lift : atom -> t      
        val mk : constr -> t      
        val mk_true : t
        val mk_and : t -> t -> t
        val mk_or : t -> t -> t
        val neg : t -> t
        val implies : t -> t -> t
        val constraints : t -> constr list

        val map_polynomial : (polynomial -> polynomial) -> t -> t
          
        (** Creates a constraint that expresses the equality of the two polynomials. *)
        val mk_eq : polynomial -> polynomial -> t
        val mk_gt : polynomial -> polynomial -> t
        val mk_ge : polynomial -> polynomial -> t
        val mk_lt : polynomial -> polynomial -> t
        val mk_le : polynomial -> polynomial -> t
        val mk_uneq : polynomial -> polynomial -> t
        val le_than_any : polynomial -> polynomial list -> t
        val le_than_all : polynomial -> polynomial list -> t
          
        val all : t list -> t
        val any : t list -> t

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

        (** Following methods return certain properties of the formula. *)
          
        (** Returns the set of variables which are active in the formula.
            A variable is active, if it's value has an effect on the evaluation of the constraint. *)
        val vars : t -> VarSet.t

        val to_string : t -> string


        (** Following methods manipulate atoms and return the manipulated versions. *)

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
