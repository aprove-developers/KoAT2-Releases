
open ProgramTypes
open Batteries
open BoundsInst

let nonprob_incoming_size program appr gt v =
  Program.pre_gt program gt
  |> GeneralTransitionSet.enum
  |> Enum.map (TransitionSet.enum % GeneralTransition.transitions)
  |> Enum.flatten
  |> Enum.filter (Location.equal (GeneralTransition.start gt) % Transition.target)
  |> Enum.map (fun t -> Bound.abs_bound (fun k -> Approximation.sizebound k appr t v))
  |> Bound.maximum
  |> RealBound.of_intbound