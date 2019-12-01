(** Provied Methods to perform a bottom-up Analysis step *)

(** Performs a bottom-up-step given a Transition ID counter, a program, an already existing approximation
    and a function find_exp_bounds to perform the analysis of the subprogram.
    Returns the modified program and an initial approximation for further analysis of the new modified program.
*)
val perform_bottom_up: generate_invariants: (Program.t -> Program.t)
                    -> find_exp_bounds:(CacheManager.t -> Program.t -> Approximation.t -> Program.t * Approximation.t)
                    -> (TransitionLabel.trans_id_counter) -> Program.t -> Approximation.t
                    -> (Program.t * Approximation.t) option
