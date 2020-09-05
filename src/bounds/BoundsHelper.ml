
open ProgramTypes
open Batteries
open BoundsInst
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

let nonprob_incoming_size pre_cache program appr gt v =
  Program.pre_gt pre_cache program gt
  |> GeneralTransitionSet.enum
  |> Enum.map (TransitionSet.enum % GeneralTransition.transitions)
  |> Enum.flatten
  |> Enum.filter (Location.equal (GeneralTransition.start gt) % Transition.target)
  |> Enum.map (fun t -> Bound.abs_bound (fun k -> Approximation.sizebound k appr t v))
  |> Bound.maximum
  |> RealBound.of_intbound
