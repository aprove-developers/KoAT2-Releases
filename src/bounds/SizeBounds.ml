open Batteries

module TransitionSet = Set.Make(Program.Transition)
             
(* Returns the maximum of all incoming sizebounds applicated to the local sizebound.
       Corresponds to 'SizeBounds for trivial SCCs':
       S'(alpha) = max{ S_l(alpha)(S(t',v_1),...,S(t',v_n)) | t' in pre(t) } *)
let highest_incoming_bound (program: Program.t)
                           (appr: Approximation.t)
                           (local_sizebound: Bound.t)
                           (t: Program.Transition.t)
    : Bound.t =
  let substitute_with_prevalues t' = Bound.substitute_f (Approximation.sizebound `Upper appr t') local_sizebound in
     t
  |> Program.pre program
  |> TransitionSet.to_list
  |> List.map substitute_with_prevalues
  |> Bound.maximum

(* Improves a trivial scc. That is an scc which consists only of one result variable.
       Corresponds to 'SizeBounds for trivial SCCs'. *)
let improve_trivial_scc (program: Program.t)
                        (appr: Approximation.t)
                        (t,v)
    : Approximation.t =
  let (local_sizebound: Bound.t) = LocalSizeBound.(as_bound (sizebound_local `Upper (Program.Transition.label t) v)) in
  let newbound =
    if Program.is_initial program t then
      local_sizebound
    else highest_incoming_bound program appr local_sizebound t
  in Approximation.add_sizebound `Upper newbound t v appr

(* Returns all bounds for result variables entering the scc. *)
let incoming_bounds (rvg: Program.RVG.t)
                    (appr: Approximation.t)
                    (scc: Program.RV.t list)
  : Bound.t Enum.t =
     Program.RVG.entry_points rvg scc
  |> Enum.map (fun (t,v) -> Approximation.sizebound `Upper appr t v)

(* Computes the maximum of the enum if non-empty, else returns None. *)
let max (enum: int Enum.t): int Option.t =
  let f = function
    | (None, y) -> Some y
    | (Some x, y) -> if x > y then Some x else Some y in
  Enum.fold (curry f) None enum

(* Computes for each transition max{s_alpha | alpha in C_t} and multiplies the results. *)
let maximal_scaling_factor (ct: Program.RV.t Enum.t)
    : int =
  ct
  |> Enum.map (LocalSizeBound.sizebound_local_rv `Upper)
  |> Enum.map Option.get (* Should exist *)
  |> Enum.map (fun (LocalSizeBound.ScaledSum (s, _, _)) -> s)
  |> max
  |? 1

let scc_variables (rvg: Program.RVG.t)
                  (scc: Program.RVG.scc)
                  (rv: Program.RV.t)
    : Var.t Enum.t =
  Program.RVG.pre rvg rv
  |> Enum.filter (fun pre ->
         List.mem_cmp Program.RV.compare pre scc (* TODO Possible performance issue *)
       )
  |> Enum.map (fun (t,v) -> v)
  
(* Computes for each transition max{ |pre(alpha)| intersected with C | alpha in C_t } and multiplies the results. *)
let maximal_affecting_scc_variables (rvg: Program.RVG.t)
                                    (scc: Program.RVG.scc)
                                    (ct: Program.RV.t Enum.t)
    : int =
  ct
  |> Enum.map (scc_variables rvg scc)
  (* Filter also all pre variables that can only have a negative effect. *)
  |> Enum.map Enum.count
  |> max
  |? 1

let transition_scaling_factor (rvg: Program.RVG.t)
                              (appr: Approximation.t)
                              (scc: Program.RVG.scc)
                              (ct: Program.RV.t Enum.t)
    : Bound.t =
  let (transition, _) = Option.get (Enum.peek ct) (* We require ct to be non-empty *) in
  Bound.exp (OurInt.of_int (maximal_scaling_factor ct *
                              maximal_affecting_scc_variables rvg scc (Enum.clone ct)))
            (Approximation.timebound `Upper appr transition) 
  

let overall_scaling_factor (rvg: Program.RVG.t)
                           (appr: Approximation.t)
                           (scc: Program.RVG.scc)
    : Bound.t =
  scc
  |> List.enum
  |> Enum.group_by (fun (t1, v1) (t2, v2) -> Program.Transition.equal t1 t2)
  |> Enum.map (transition_scaling_factor rvg appr scc)
  |> Enum.fold Bound.( * ) Bound.one

let incoming_vars_effect (rvg: Program.RVG.t)
                         (appr: Approximation.t)
                         (scc: Program.RV.t list)
                         (vars: VarSet.t)
                         (transition: Program.Transition.t)
                         (alpha: Program.RV.t)
    : Bound.t =
  vars
  |> VarSet.enum
  (* TODO Performance issue *)
  |> Enum.filter (fun v -> not (Enum.exists (Var.equal v) (scc_variables rvg scc alpha)))
  |> Enum.map (fun v ->
         Program.RVG.pre rvg alpha 
         |> Enum.filter (fun rv -> not (Enum.exists (Program.RV.equal rv) (List.enum scc)))
         |> Enum.filter (fun (t,v') -> Var.equal v v')
         |> Enum.map (fun (t,v) -> Approximation.sizebound `Upper appr t v)
         |> Enum.fold Bound.max Bound.minus_infinity
       )
  |> Enum.fold Bound.(+) Bound.zero
  
let transition_effect (rvg: Program.RVG.t)
                      (appr: Approximation.t)
                      (scc: Program.RV.t list)
                      (ct: Program.RV.t Enum.t)
                      (transition: Program.Transition.t)
    : Bound.t =
  ct
  |> Enum.map (fun alpha -> (alpha, Option.get (LocalSizeBound.sizebound_local_rv `Upper alpha)))
  (* Should exist *)
  |> Enum.map (fun (alpha, LocalSizeBound.ScaledSum (_, e, vars)) ->
         Bound.add (Bound.of_int e) (incoming_vars_effect rvg appr scc vars transition alpha)
       )
  |> Enum.fold Bound.max Bound.minus_infinity

let effects (rvg: Program.RVG.t)
            (appr: Approximation.t)
            (scc: Program.RV.t list)
    : Bound.t =
  scc
  |> List.enum
  |> Enum.group_by (fun (t1, v1) (t2, v2) -> Program.Transition.equal t1 t2)
  |> Enum.map (fun ct ->
         let (transition, _) = Option.get (Enum.peek ct) (* We require ct to be non-empty *) in
         Bound.(Approximation.timebound `Upper appr transition * transition_effect rvg appr scc ct transition)
       )
  |> Enum.fold Bound.add Bound.zero

let get_all (xs: ('a Option.t) list): ('a list) Option.t =
  let combine result maybe =
    Option.bind maybe (fun x -> Option.map (fun list -> x :: list) result) in
  List.fold_left combine (Some []) xs 

(* Improves a nontrivial scc. That is an scc which consists of more than one result variable.
       Corresponds to 'SizeBounds for nontrivial SCCs'. *)
let improve_nontrivial_scc (program: Program.t)
                           (rvg: Program.RVG.t)
                           (appr: Approximation.t)
                           (scc: Program.RV.t list)
    : Approximation.t =
  scc
  |> List.map (LocalSizeBound.sizebound_local_rv `Upper)
  |> get_all
  |> Option.map (fun lsbs -> Bound.(overall_scaling_factor rvg appr scc * effects rvg appr scc))
  |> Option.map (fun new_bound -> Approximation.add_sizebounds `Upper new_bound scc appr)
  |? appr
         
(* Improves a whole scc. *)
let improve_scc (program: Program.t)
                (rvg: Program.RVG.t)
                (appr: Approximation.t)
                (scc: Program.RV.t list)
    : Approximation.t  =
  match scc with
  | [rv] -> improve_trivial_scc program appr rv
  | scc -> improve_nontrivial_scc program rvg appr scc
         
let improve program appr =
  let module C = Graph.Components.Make(Program.RVG) in
  let rvg = Program.rvg program in
  List.fold_left (fun appr scc -> improve_scc program rvg appr scc) appr (C.scc_list rvg)
