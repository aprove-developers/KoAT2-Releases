open! OurBase
open Bounds

type ('class_bound, 'prob_bound) kind =
  | PAST : (Bound.t, RationalBound.t) kind
  | AST : (BinaryBound.t, BinaryBound.t) kind

module type T = sig
  module ClassBound : BoundType.Bound
  module ProbBound : BoundType.Bound

  val kind : (ClassBound.t, ProbBound.t) kind
  val class_to_prob_bound : ClassBound.t -> ProbBound.t
  val prob_to_class_bound : ProbBound.t -> ClassBound.t
end

module PAST = struct
  module ClassBound = Bound
  module ProbBound = RationalBound

  let kind = PAST
  let class_to_prob_bound = RationalBound.of_intbound
  let prob_to_class_bound = RationalBound.to_intbound
end

module AST = struct
  module ClassBound = BinaryBound
  module ProbBound = BinaryBound

  let kind = AST
  let class_to_prob_bound = identity
  let prob_to_class_bound = identity
end
