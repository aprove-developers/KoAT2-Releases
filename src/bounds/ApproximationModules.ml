open Batteries
open ProgramModules

module RV = RVGTypes.RV

module TransitionApproximation = TransitionApproximationType.Make_TransitionApproximation(OurInt)(Polynomials.Polynomial)
                                                                                         (struct
                                                                                           include Transition
                                                                                           let fold_transset = TransitionSet.fold
                                                                                          end)


module SizeApproximation =
  SizeApproximationType.Make_SizeApproximation(OurInt)(Polynomials.Polynomial)
                                              (struct
                                                include Transition
                                                let target_string =
                                                  Location.to_string % Transition.target
                                               end)
                                              (RV)
