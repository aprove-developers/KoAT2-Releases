open Batteries
open BoundsInst
open Formulas
open Polynomials
open ProgramTypes

let logger = Logging.(get LocalSizeBound)

module Solver = SMT.IncrementalZ3SolverInt

(** Performs a binary search between the lowest and highest value to find the optimal value which satisfies the predicate.
    We assume that the highest value already satisfies the predicate.
    Therefore this method always finds a solution. *)
let rec binary_search ?(divisor=2.) (lowest: int) (highest: int) (p: int -> bool) =
  if lowest >= highest then
    lowest
  else
    (* We need to ensure that the result is always round down to prevent endless loops.
       Normal integer division rounds towards zero. *)
    let newBound = Float.to_int (Float.floor (Float.div (Float.of_int (lowest + highest)) divisor)) in
    if p newBound then
      binary_search ~divisor:(if newBound < 0 then 2. else divisor) lowest newBound p
    else
      binary_search ~divisor:(if newBound < 0 then divisor else 2.) (newBound + 1) highest p

(* For 's' it is sufficient to only view the max occurring constants of the update polynomial. *)
let s_range update =
  update
  |> Polynomial.max_of_occurring_constants
  |> OurInt.max (OurInt.of_int 1) (* 0 or lower is not allowed *)
  |> OurInt.min (OurInt.of_int 1024) (* TODO We cut it at the moment at 1024, because sometimes the approximation is worse than an integer value. *)
  |> OurInt.to_int

(* For 'c' we want to view the max occurring constants of the complete formula *)
let c_range formula =
  formula
  |> Formula.max_of_occurring_constants
  |> OurInt.min (OurInt.of_int 1024) (* TODO We cut it at the moment at 1024, because sometimes the approximation is worse than an integer value. *)
  |> OurInt.to_int

type t = {
  factor: int;
  constant: int;
  vars: VarSet.t;
} [@@deriving eq]

let mk ?(s=1) ?(c=0) vars = 
{
  factor = abs(s);
  constant = abs(c);
  vars = (VarSet.of_string_list vars)

}

let initial_lsb s c vs = {factor = s; constant = c; vars = vs;}

let factor t = t.factor

let constant t = t.constant

let vars t = t.vars

let to_string lsb =
  "{" ^
  "factor: " ^ Int.to_string lsb.factor ^ "; " ^
  "constant: " ^ Int.to_string lsb.constant ^ "; " ^
  "vars: " ^ VarSet.to_string lsb.vars ^ "; " ^
  "}"

let to_string_option = function
  | None -> "Unbounded"
  | Some lsb -> to_string lsb

let as_bound lsb =
  let vars_sum = Bound.sum @@ Enum.map Bound.of_var (VarSet.enum lsb.vars) in
  Bound.(of_int lsb.factor * (of_int lsb.constant + vars_sum))

let option_lsb_as_bound = function
  | Some a -> as_bound a
  | None -> Bound.infinity

let as_substituted_bound substitution = Bound.substitute_f substitution % as_bound

let is_bounded_with solver update_formula v' t =
  (* Prove that under formula the bound from validity_as_bound always evaluates to a non-negative value *)
  Solver.push solver;
  (* Check if as_bound is always greator or equal than v' *)
  Solver.add_bound_comparison solver `LT (as_bound t) (Bound.of_var v');
  let result = Solver.unsatisfiable solver in
  Solver.pop solver;
  result

let optimize_s max_s predicate lsb =
  let s_result =
    binary_search ~divisor:16. 1 max_s
      (fun next_s -> predicate ( {lsb with factor = next_s}))
  in
  {lsb with factor = s_result}

let optimize_c max_c predicate lsb =
  let c_result =
    binary_search ~divisor:16. 0 max_c
      (fun next_c -> predicate ( {lsb with constant = next_c}))
  in
  {lsb with constant = c_result}

let find_bound update_vars v' update_formula max_s =
  let solver = Solver.create ~model:false () in
  let max_c = c_range update_formula in
  let is_bounded b = is_bounded_with solver update_formula v' b in
  Enum.seq 0 ((+) 1) ((>) (VarSet.cardinal update_vars + 1))
  |> Enum.map (fun count ->
      List.enum (VarSet.combinations count update_vars)
     )
  |> Enum.flatten
  |> Enum.map (initial_lsb max_s max_c)
  |> Enum.filter is_bounded
  |> Enum.map (optimize_s max_s is_bounded)
  |> Enum.map (optimize_c max_c is_bounded)
  |> Enum.peek

let compute_bound program_vars (l,t,l') var =
  let execute () =
    TransitionLabel.update t var
    |> flip Option.bind (fun ue ->
        let v' = Var.fresh_id Var.Int () in
        let update_formula = Formula.Infix.(Formula.mk (TransitionLabel.guard t) && Polynomial.of_var v' = ue) in
        let update_vars =
          VarSet.union
           (Polynomial.vars ue)
           (VarSet.inter (VarSet.singleton var) (TransitionLabel.Guard.vars @@ TransitionLabel.guard t))
        in
        (* We have to intersect update_vars with the program vars in order to eliminate temporary variables from local size bounds*)
        find_bound (VarSet.inter program_vars update_vars) v' update_formula (s_range ue)
       )
  in
  Logger.with_log logger Logger.DEBUG
      (fun () -> "compute_bound", [ "transition", Transition.to_id_string (l,t,l')
                                  ; "var", Var.to_string var])
      ~result:to_string_option
      execute

(** Internal memoization for local size bounds *)
module LSB_Cache =
  Hashtbl.Make(
      struct
        type t = Transition.t * Var.t
        let equal (t1,v1) (t2,v2) =
          Transition.same t1 t2
          && Var.equal v1 v2
        let hash (t,v) =
          Hashtbl.hash (Transition.id t, v)
      end
    )

type lsb_cache = t Option.t LSB_Cache.t

let (table: lsb_cache) =
  LSB_Cache.create 10

let reset () =
  LSB_Cache.clear table

let sizebound_local program t v =
  let program_vars = Program.input_vars program in
  let cache = table in
  match LSB_Cache.find_option cache (t,v) with
    | Some lsb -> lsb
    | None ->
        let lsb = compute_bound program_vars t v in
        LSB_Cache.add cache (t,v) lsb;
        lsb

let sizebound_local_rv program (t,v) =
  sizebound_local program t v

let sizebound_local_scc program scc: (Transition.t * Var.t -> t) Option.t =
  let lsbs =
    List.map (fun (t,v) -> (t,v), sizebound_local program t v) scc
  in
  if List.for_all (Option.is_some % Tuple2.second) lsbs then
    Some (fun k -> Option.get @@ List.assoc k lsbs)
  else None


(** Internal memoization for local size bounds.
  The idea is to use this cache if we applied cfr and
    1) delete it and use the original cache if we get a timeout or
    2) if the analysis of the unrolled scc is completed successfully use this cache as the main memory. *)
let currently_cfr = ref false

module LSB_Cache_cfr =
  Hashtbl.Make(
      struct
        type t = Transition.t * Var.t
        let equal (t1,v1) (t2,v2) =
          Transition.same t1 t2
          && Var.equal v1 v2
        let hash (t,v) =
          Hashtbl.hash (Transition.id t, v)
      end
    )

let  (table_cfr: t Option.t LSB_Cache_cfr.t) =
  LSB_Cache_cfr.create 10

let reset_cfr () =
  currently_cfr := false;
  LSB_Cache_cfr.clear table_cfr

let switch_cache () =
  if !currently_cfr then (
    reset();
    Enum.iter (fun (k,b) -> LSB_Cache.add table k b) (LSB_Cache_cfr.enum table_cfr);
    reset_cfr()
  )

let enable_cfr () =
  currently_cfr := true


