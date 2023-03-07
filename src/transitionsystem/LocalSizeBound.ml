open Batteries
open BoundsInst
open Formulas
open Polynomials
open Constraints

let logger = Logging.(get LocalSizeBound)

module Solver = SMT.IncrementalZ3Solver

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


module Make(TL: ProgramTypes.TransitionLabel with type update_element = Polynomial.t)
           (T: ProgramTypes.Transition with type transition_label = TL.t)
           (P: ProgramTypes.Program with type transition_label = TL.t) = struct

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

  let is_constant = VarSet.is_empty % vars

  let to_string lsb =
    "{" ^
    "factor: " ^ Int.to_string lsb.factor ^ "; " ^
    "constant: " ^ Int.to_string lsb.constant ^ "; " ^
    "vars: " ^ VarSet.to_string lsb.vars ^ "; " ^
    "}"

  let to_string_option = function
    | None -> "Unbounded"
    | Some lsb -> to_string lsb

  let to_string_option_tuple = function
    | None -> "Unbounded"
    | Some (lsb,b) -> to_string lsb ^ " equality: " ^ Bool.to_string (Lazy.force b)

  let as_bound lsb =
    let vars_sum = Bound.sum @@ Enum.map Bound.of_var (VarSet.enum lsb.vars) in
    Bound.(of_int lsb.factor * (of_int lsb.constant + vars_sum))

  let option_lsb_as_bound = function
    | Some a -> as_bound a
    | None -> Bound.infinity

  let is_bounded_with solver update_formula v' t =
    (* Prove that under formula the bound from validity_as_bound always evaluates to a non-negative value *)
    Solver.push solver;
    (* Check if as_bound is always greator or equal than v' *)
    Solver.add_bound_comparison solver `LT (as_bound t) (Bound.of_var v');
    let result = Solver.unsatisfiable solver in
    Solver.pop solver;
    result

  let is_of_equality_type t update_formula v' =
    (* Trivially holds for constant lsbs *)
    if VarSet.is_empty t.vars then true
    else
      (* Trivially holds for identity lsbs *)
      if t.factor > 1 && not (VarSet.is_empty t.vars) then false
      else
      (* Trivially does not hold if scaling > 1 and variables are present *)
        if VarSet.cardinal t.vars = 1 && Int.equal 0 t.constant then true
        else
          if Formula.is_linear update_formula then
            let solver = Solver.create ~model:false () in
            (* Find contra *)
            Solver.add solver update_formula;
            VarSet.to_list t.vars
            |> List.iter (fun v -> Solver.add_bound_comparison solver `LT (Bound.of_var v) (Bound.of_var v'));
            Solver.add_bound_comparison solver `LT (Bound.of_int t.constant) (Bound.of_var v');
            let contra_exists = Solver.satisfiable solver in
            not contra_exists
          else
            false

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
    let max_c = c_range update_formula in
    let execute () =
      let solver = Solver.create ~model:false () in
      Solver.add solver update_formula;
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
      |> Option.map (fun t -> t, Lazy.from_fun (fun () -> is_of_equality_type t update_formula v'))
    in
    Logger.with_log logger Logger.DEBUG
      (fun () -> "find_bound", [ "update_vars", VarSet.to_string update_vars
                              ; "v'", Var.to_string v'
                              ; "max_s", Int.to_string max_s
                              ; "max_c", Int.to_string max_c
                              ; "update_formula", Formula.to_string update_formula])
      ~result:(to_string_option % Option.map Tuple2.first)
      execute

  let compute_bound program_vars (l,t,l') var =
    let execute () =
      TL.update t var
      |> flip Option.bind (fun ue ->
          let v' = Var.fresh_id Var.Int () in
          let update_formula =
            (* Facilitate SMT call by removing non-linear constraints. *)
            (* The resulting update_formula is an overapproximation of the original formula *)
            Formula.mk @@ Constraint.drop_nonlinear @@
              (Constraint.mk_and (TL.guard t) (Constraint.mk_eq (Polynomial.of_var v') ue))
          in
          let update_vars =
            VarSet.union
            (Polynomial.vars ue)
            (VarSet.inter (VarSet.singleton var) (Guard.vars @@ TL.guard t))
          in
          try (* thrown if solver does not know a solution due to e.g. non-linear arithmetic *)
            (* We have to intersect update_vars with the program vars in order to eliminate temporary variables from local size bounds*)
            find_bound (VarSet.inter program_vars update_vars) v' update_formula (s_range ue)
          with
            SMT.SMTFailure _ -> None
        )
    in
    Logger.with_log logger Logger.DEBUG
        (fun () -> "compute_bound", [ "transition", T.to_id_string (l,t,l')
                                    ; "guard", Constraints.Constraint.to_string (TL.guard t)
                                    ; "var", Var.to_string var])
        ~result:to_string_option_tuple
        execute

  let sizebound_local_with_equality program t v =
    compute_bound (P.input_vars program) t v

  let sizebound_local program t v =
      Option.map Tuple2.first @@ sizebound_local_with_equality program t v

end

include Make(TransitionLabel_)(Transition_)(Program_)
