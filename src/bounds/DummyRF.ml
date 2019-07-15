open Batteries
open Polynomials
open ProgramTypes

module Make = 
    struct
        type mrf = (Location.t -> Polynomial.t) list


        module SMTSolver = SMT.Z3Solver
        module Valuation = Valuation.Make(OurInt)

        type measure = [ `Cost | `Time ] [@@deriving show, eq]

        let logger = Logging.(get PRF)

        let decreaser measure t =
            match measure with
            | `Cost -> TransitionLabel.cost t
            | `Time -> Polynomial.one


        (* method transforms polynome to parapolynom*)
        let as_parapoly label var =
            match TransitionLabel.update label var with
            (** Correct? In the nondeterministic case we just make it deterministic? *)
            | None -> ParameterPolynomial.of_var var
            | Some p -> ParameterPolynomial.of_polynomial p


        (** Given a list of variables an affine template-polynomial is generated*)
        let ranking_template (vars: VarSet.t): ParameterPolynomial.t * Var.t list =
            let vars = VarSet.elements vars in
            let num_vars = List.length vars in
            let fresh_vars = Var.fresh_id_list Var.Int num_vars in
            let fresh_coeffs = List.map Polynomial.of_var fresh_vars in
            let linear_poly = ParameterPolynomial.of_coeff_list fresh_coeffs vars in
            let constant_var = Var.fresh_id Var.Int () in
            let constant_poly = ParameterPolynomial.of_constant (Polynomial.of_var constant_var) in
            ParameterPolynomial.(linear_poly + constant_poly),
            List.append fresh_vars [constant_var]

        module TemplateTable = Hashtbl.Make(Location)

        module RankingTable = Hashtbl.Make(struct include Transition let equal = Transition.same end)
        module Solver = SMT.IncrementalZ3Solver

    end