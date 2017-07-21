open Batteries
open PolyTypes

module type PolynomialConstraintsAtom =
    sig
        type var
        type polynomial
        type rename_map
        type value
        type valuation
        type constraint_atom_ast
        type t
        (*getting information*)
        val get_first_arg : t -> polynomial
        val get_second_arg : t -> polynomial
        
        (*creation*)
        val mk_gt : polynomial -> polynomial -> t
        val mk_ge : polynomial -> polynomial -> t
        val mk_lt : polynomial -> polynomial -> t
        val mk_le : polynomial -> polynomial -> t
        val mk_eq : polynomial -> polynomial -> t
        val mk_neq : polynomial -> polynomial -> t
        
        (*boolean tests*)
        val is_gt : t -> bool
        val is_ge : t -> bool
        val is_lt : t -> bool
        val is_le : t -> bool
        val is_eq : t -> bool
        val is_neq : t -> bool
        val is_same_constr : t -> t -> bool
        val is_inverted_constr : t -> t -> bool
        val is_redundant : t -> t -> bool
        val (==) : t -> t -> bool
        
        (*export*)
        val to_string : t -> string
        val to_z3 : Z3.context -> t -> Z3.Expr.expr
        val get_variables : t -> var list
        val rename_vars : t -> rename_map -> t
        val eval_bool : t -> valuation -> bool
        val from_ast_atom : constraint_atom_ast -> t
    end
    
module PolynomialConstraintsAtomAST(Var : ID) =
    struct
        module PolynomialAST = PolynomialAST(Var)
        type polynomial = PolynomialAST.t
        type t =
            | LessThan of polynomial * polynomial
            | LessEqual of polynomial * polynomial
            | GreaterThan of polynomial * polynomial
            | GreaterEqual of polynomial * polynomial
            | Equal of polynomial * polynomial
            | Neq of polynomial * polynomial
end