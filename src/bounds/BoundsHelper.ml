
open ProgramTypes
open Batteries
open BoundsInst
open SMT
open Formulas


type comperator = [`GT | `GE]

let comperator_equal cmp cmp' = match (cmp,cmp') with
  | (`GT,`GT) -> true
  | (`GE,`GE) -> true
  | _         -> false

module BoundTable = Hashtbl.Make (struct
                                    type t = RealFormula.t * comperator * RealBound.t * RealBound.t

                                    let equal (f,c,b1,b2) (f',c',b1',b2') =
                                      comperator_equal c c'
                                      && String.equal (RealFormula.to_string f) (RealFormula.to_string f')
                                      && RealBound.equal b1 b1'
                                      && RealBound.equal b2 b2'
                                    let hash  = Hashtbl.hash
                                  end)

let cacheTable: bool BoundTable.t = BoundTable.create 20

let nonprob_incoming_size program appr gt v =
  Program.pre_gt program gt
  |> GeneralTransitionSet.enum
  |> Enum.map (TransitionSet.enum % GeneralTransition.transitions)
  |> Enum.flatten
  |> Enum.filter (Location.equal (GeneralTransition.start gt) % Transition.target)
  |> Enum.map (fun t -> Bound.abs_bound (fun k -> Approximation.sizebound k appr t v))
  |> Bound.maximum
  |> RealBound.of_intbound

let simplify_bound_with_smt logger invariants b =
  let cached_z3 invariants cmp b1 b2 =
    SMT.Z3Solver.cmp_bounds invariants cmp b1 b2
  in

  let helper cmp b1 b2 =
    try
      match cached_z3 invariants cmp b1 b2 with
      | true -> Some true
      | _    ->
        (* Check the other direction, this is not necessarily implied by the previous step. E.g. we have not 1<X and also not neg X>1 for no invariants*)
        let res = match cmp with
          | `GE -> cached_z3 invariants `GT b2 b1
          | `GT -> cached_z3 invariants `GE b2 b1
        in
        match res with
        | true -> Some false
        | _    -> None

    with _ -> None
  in
  let execute () =
    (if VarSet.disjoint (Formulas.RealFormula.vars invariants) (RealBound.vars b) then
      b
    else
      RealBound.simplify_opt_invariants helper b)
    |> RealBound.substitute_f RealBound.(abs % of_var)
  in
  Logger.with_log logger Logger.DEBUG
    (fun () -> "simplify_bound_with_smt", ["invariants", Formulas.RealFormula.to_string invariants;"bound", RealBound.to_string b])
    ~result:(RealBound.to_string)
    execute
