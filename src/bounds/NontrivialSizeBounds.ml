open Batteries
open Program.Types

let logger = Logging.(get Size)

type kind = [ `Lower | `Upper ] [@@deriving show]

let transition_rvs (scc: RV.t list): (Transition.t * Var.t list) list =
  scc
  |> List.group_consecutive (fun (t1, v1) (t2, v2) -> Transition.equal t1 t2)
  |> List.map (fun rvs -> ((Tuple2.first % List.hd) rvs, List.map Tuple2.second rvs))
  
(** Returns all the variables that may influence the given alpha and get changed in the scc. *)
let scc_variables rvg scc rv =
  RVG.pre rvg rv
  |> Util.intersection RV.equal (List.enum scc)
  |> Enum.map (fun (t,v) -> v)
  
(** Computes for each transition max(abs(pre(alpha)) intersected with C for all alpha in C_t) and multiplies the results. *)
(* TODO Kind only relevant for only positive and only negative effects. *)
let extreme_affecting_scc_variables kind rvg scc (t, variables) =
  let execute () =
    variables
    |> List.enum
    |> Enum.map (fun v -> scc_variables rvg scc (t,v))
    (* Filter also all pre variables that can only have a negative effect. *)
    |> Enum.map Enum.count
    |> Util.max_option (>)
    |? 1
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "extreme affecting scc variables", ["scc", RVG.rvs_to_id_string scc])
                     ~result:Int.to_string
                     execute

(** Computes for each transition max(s_alpha for all alpha in C_t) and multiplies the results. *)
let extreme_scaling_factor kind get_lsb (t, variables) =
  let execute () =
    variables
    |> List.enum
    |> Enum.map (fun v -> get_lsb kind (t,v))
    |> Enum.map LocalSizeBound.factor
    |> Util.max_option (>)
    |? 1
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "extreme scaling factor", ["transition", Transition.to_id_string t;
                                                           "variables", (VarSet.to_string % VarSet.of_list) variables])
                     ~result:Int.to_string
                     execute

(** The transition scaling factor corresponds to (s_t)^R(t) *)
let transition_scaling_factor kind rvg get_lsb get_timebound scc (t, variables) =
  let execute () =
    Bound.exp
      (OurInt.of_int (extreme_scaling_factor kind get_lsb (t, variables) * extreme_affecting_scc_variables kind rvg scc (t, variables)))
      (get_timebound t)
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "transition scaling factor", ["transition", Transition.to_id_string t;
                                                              "variables", (VarSet.to_string % VarSet.of_list) variables;
                                                              "scc", RVG.rvs_to_id_string scc])
                     ~result:Bound.to_string
                     execute
  
(** The overall scaling factor is defined as the product over all scaling factors of transitions in the scc, 
    each scaling factor with the timebound of the transition as exponent.
    prod_{t in T} (s_t)^R(t) *)
let overall_scaling_factor kind rvg get_lsb get_timebound scc =
  let execute () =
    scc
    |> transition_rvs
    |> List.enum
    |> Enum.map (transition_scaling_factor kind rvg get_lsb get_timebound scc)
    |> Bound.product
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "overall scaling factor", ["scc", RVG.rvs_to_id_string scc])
                     ~result:Bound.to_string
                     execute

let incoming_vars_effect kind rvg get_sizebound scc vars alpha =
  let execute () =
    vars
    |> VarSet.enum
    |> Util.without Var.equal (scc_variables rvg scc alpha)
    |> Enum.map (fun v ->
           RVG.pre rvg alpha
           |> Util.without RV.equal (List.enum scc)
           |> Enum.filter (fun (t,v') -> Var.equal v v')
           |> Enum.map (fun (t,v) -> get_sizebound kind t v)
           |> Bound.maximum
         )
    |> Bound.sum
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "incoming vars effect", ["scc", RVG.rvs_to_id_string scc;
                                                         "vars", VarSet.to_string vars;
                                                         "alpha", RV.to_id_string alpha])
                     ~result:Bound.to_string
                     execute

(** The highest effect of the specific transition in the scc.
    A transition can manipulate different variables.
    The result of this function is for all those variables the highest incoming value. *)  
let transition_effect kind rvg get_lsb get_sizebound scc t variables =
  let execute () =
    variables
    |> List.enum
    |> Enum.map (fun v ->
           Bound.(max zero (of_int (LocalSizeBound.constant (get_lsb kind (t,v))) +
                              incoming_vars_effect kind rvg get_sizebound scc (LocalSizeBound.vars (get_lsb kind (t,v))) (t,v)))
         )
    |> Bound.maximum
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "transition effect", ["scc", RVG.rvs_to_id_string scc;
                                                      "variables", (VarSet.to_string % VarSet.of_list) variables;
                                                      "transition", Transition.to_id_string t])
                     ~result:Bound.to_string
                     execute

let effects kind rvg get_lsb get_timebound get_sizebound scc =
  let execute () =
    scc
    |> transition_rvs
    |> List.map (fun (t,variables) ->
           Bound.(get_timebound t * transition_effect kind rvg get_lsb get_sizebound scc t variables)
         )
    |> List.enum
    |> Bound.sum
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "effects", ["scc", RVG.rvs_to_id_string scc])
                     ~result:Bound.to_string
                     execute

let sign = function
  | `Lower -> Bound.neg
  | `Upper -> identity
  
(** Computes a bound for a nontrivial scc. That is an scc which consists of a loop.
    Corresponds to 'SizeBounds for nontrivial SCCs'. *)
let compute kind program rvg get_timebound get_sizebound scc =
  let execute () =
    LocalSizeBound.sizebound_local_scc kind scc
    |> Option.map (fun get_lsb ->
           Bound.(overall_scaling_factor kind rvg get_lsb get_timebound scc * effects kind rvg get_lsb get_timebound get_sizebound scc)
           |> sign kind
         )
    |? match kind with
       | `Lower -> Bound.minus_infinity
       | `Upper -> Bound.infinity
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute nontrivial bound", ["kind", show_kind kind;
                                                             "scc", RVG.rvs_to_id_string scc])
                     ~result:Bound.to_string
                     execute
