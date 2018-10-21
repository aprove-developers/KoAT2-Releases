open Batteries
open ProgramTypes

module RV = RVGTypes.Make_RV(Transition)
module ERV = RVGTypes.Make_RV(RVTransitions.TransitionForExpectedSize)

module TransitionApproximation = TransitionApproximationType.Make_TransitionApproximation(OurInt)(Polynomials.Polynomial)
                                                                                         (struct
                                                                                           include Transition
                                                                                           let fold_transset = TransitionSet.fold
                                                                                          end)

module GeneralTransitionApproximation =
  TransitionApproximationType.Make_TransitionApproximation(OurFloat)(Polynomials.RealPolynomial)
                                                          (struct
                                                            include GeneralTransition
                                                            let fold_transset fold_func tset start_val =
                                                              GeneralTransitionSet.from_transitionset tset
                                                              |> fun gtset -> GeneralTransitionSet.fold fold_func gtset start_val
                                                            let compare_same = compare
                                                           end)

module SizeApproximation =
  SizeApproximationType.Make_SizeApproximation(OurInt)(Polynomials.Polynomial)
                                              (struct
                                                include Transition
                                                let target_string =
                                                  Location.to_string % Transition.target
                                               end)
                                              (RVGTypes.Make_RV (Transition))


module ExpectedSizeApproximation =
  SizeApproximationType.Make_SizeApproximation(OurFloat)(Polynomials.RealPolynomial)
                                              (RVTransitions.TransitionForExpectedSize)
                                              (RVGTypes.Make_RV
                                                (RVTransitions.TransitionForExpectedSize))
