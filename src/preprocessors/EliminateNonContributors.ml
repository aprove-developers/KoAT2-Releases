(** Implemenation of a preprocessor which eliminates variables that do not contribute to guards. *)
open Batteries
open Formulas
open ProgramTypes
open Constraints
open Polynomials

let logger = Logging.(get Preprocessor)

let depends var label = 
    VarSet.exists (fun x -> TransitionLabel.update label x |? Polynomial.zero |> Polynomial.vars |> VarSet.mem var
                         || TransitionLabel.cost label |> Polynomial.vars |> VarSet.mem var)

let rec eliminate_ program contributors non_contributors = 
    let (xs,ys) = TransitionSet.fold (fun (l,t,l') (xs,ys) -> 
                    VarSet.fold (fun y (xs,ys) -> 
                        if depends y t xs then 
                            (VarSet.add y xs, VarSet.remove y ys) 
                        else (xs,ys)) ys (xs,ys)) 
                        (Program.transitions program) 
                        (contributors, non_contributors) in
    if VarSet.equal non_contributors ys then
        contributors
    else
        eliminate_ program xs ys

let eliminate program = 
    let vars = Program.vars program in
    let vars_guard = TransitionSet.fold (fun (l,t,l') xs -> VarSet.union (Constraint.vars (TransitionLabel.guard t)) xs) (Program.transitions program) VarSet.empty in
    let contributors = eliminate_ program vars_guard (VarSet.diff vars vars_guard) in
    let non_contributors = VarSet.diff vars contributors in
    let transitions_ = program |> Program.transitions |> TransitionSet.map (fun (l,t,l') -> (l,TransitionLabel.remove_non_contributors non_contributors t ,l')) in
    let program_ = Program.from (TransitionSet.to_list transitions_) (Program.start program) in
        Logger.(log logger INFO (fun () -> "EliminateNonContributors", ["non_contributors", VarSet.to_string non_contributors]));
        Logger.(log logger DEBUG (fun () -> "EliminateNonContributors", ["Program", Program.to_string program_]));
    if VarSet.is_empty non_contributors then (** this is hideous *)
        (MaybeChanged.same program)
    else
        (MaybeChanged.changed program_)
