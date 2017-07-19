open Batteries
open PolyTypes

module type PolynomialConstraintsAtom =
    sig
        type var
        type polynomial
        type rename_map
        type value
        type valuation
        type t
        
        val get_first_arg : t -> polynomial
        val get_second_arg : t -> polynomial
        val mk_gt : polynomial -> polynomial -> t
        val mk_ge : polynomial -> polynomial -> t
        val mk_lt : polynomial -> polynomial -> t
        val mk_le : polynomial -> polynomial -> t
        val mk_eq : polynomial -> polynomial -> t
        val mk_neq : polynomial -> polynomial -> t
        val is_gt : t -> bool
        val is_ge : t -> bool
        val is_lt : t -> bool
        val is_le : t -> bool
        val is_eq : t -> bool
        val is_neq : t -> bool
        val is_same_constr : t -> t -> bool
        val (==) : t -> t -> bool
        val to_string : t -> string
        val to_z3 : Z3.context -> t -> Z3.Expr.expr
        val get_variables : t -> var list
        val rename_vars : rename_map -> t -> t
        val eval : t -> valuation -> bool
    end