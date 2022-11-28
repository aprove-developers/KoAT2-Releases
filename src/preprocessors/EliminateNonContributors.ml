(** Implemenation of a preprocessor which eliminates variables that do not contribute to guards. *)
open Batteries
open Formulas
open Constraints
open Polynomials

module Make(M: ProgramTypes.ClassicalProgramModules) = struct
    open M
    let logger = Logging.(get Preprocessor)

    let depends var label =
        VarSet.exists (fun x -> (TransitionLabel.update label x |? Polynomial.zero |> Polynomial.vars |> VarSet.mem var)
                            || (TransitionLabel.cost label |> Polynomial.vars |> VarSet.mem var))


    let rec eliminate_t_ vars get_update contributors non_contributors =
        let (xs,ys) = VarSet.fold (fun y (contr,non_contr) ->
                            if  VarSet.exists (fun x -> Polynomial.vars (get_update x |? Polynomial.zero) |> VarSet.mem y) contr then
                                (VarSet.add y contr, VarSet.remove y non_contr)
                            else
                                (contr,non_contr)) vars
                            (contributors, non_contributors) in
        if VarSet.equal non_contributors ys then
            contributors
        else
            eliminate_t_ vars get_update xs ys

    let rec eliminate_ program contributors non_contributors =
        let (xs,ys) = TransitionSet.fold (fun (l,t,l') (xs,ys) ->
                        VarSet.fold (fun y (contr,non_contr) ->
                            if depends y t contr then
                                (VarSet.add y contr, VarSet.remove y non_contr)
                            else
                                (contr,non_contr)) ys (xs,ys))
                            (Program.transitions program)
                            (contributors, non_contributors) in
        if VarSet.equal non_contributors ys then
            contributors
        else
            eliminate_ program xs ys

    let eliminate_t vars vars_guard get_update remove_non_contributors =
        let init_contr = vars_guard in
        let init_non_contr = VarSet.diff vars init_contr in
        Logger.(log logger INFO (fun () -> "EliminateNonContributors", [("init_contr", VarSet.to_string init_contr);("init_non_contributors", VarSet.to_string (VarSet.diff vars vars_guard))]));
        let contributors = eliminate_t_ vars get_update init_contr init_non_contr in
        let non_contributors = VarSet.diff vars contributors in
        remove_non_contributors non_contributors

    let eliminate program =
        let vars = Program.vars program in
        let vars_guard = TransitionSet.fold (fun (l,t,l') xs -> VarSet.union (Constraint.vars (TransitionLabel.guard t)) xs) (Program.transitions program) VarSet.empty
        and vars_cost = TransitionSet.fold (fun (l,t,l') xs -> VarSet.union (Polynomial.vars (TransitionLabel.cost t)) xs) (Program.transitions program) VarSet.empty in
        let init_contr = VarSet.union vars_guard vars_cost in
        Logger.(log logger INFO (fun () -> "EliminateNonContributors", [("init_contr", VarSet.to_string init_contr);("init_non_contributors", VarSet.to_string (VarSet.diff vars vars_guard))]));
        let contributors = eliminate_ program init_contr (VarSet.diff vars init_contr) in
        let non_contributors = VarSet.diff vars contributors in
        let program_ = Program.remove_non_contributors non_contributors program in
        Logger.(log logger INFO (fun () -> "EliminateNonContributors", [("non_contributors", VarSet.to_string non_contributors)]));
        if not (VarSet.is_empty non_contributors) then
        ProofOutput.add_str_paragraph_to_proof(fun () -> "Eliminate variables "^VarSet.to_string ~pretty:true non_contributors^" that do not contribute to the problem");
        if VarSet.is_empty non_contributors then (** this is hideous *)
            MaybeChanged.same program
        else
            MaybeChanged.changed program_
end

include Make(ProgramModules)
