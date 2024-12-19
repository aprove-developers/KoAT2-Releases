open! OurBase
open FormattedString
open Lens.Infix

type ('trans_appr_type, 'size_appr_type) approximation_t = {
  time : 'trans_appr_type;
  size : 'size_appr_type;
  cost : 'trans_appr_type;
}
[@@deriving lens { submodule = true }]

module Make
    (B : BoundType.Bound)
    (PM : ProgramTypes.ProgramModules)
    (T : ApproximationTypes.ApproximableTransitionType with type program = PM.Program.t)
    (TransitionApproximation :
      ApproximationTypes.TransitionApproximationType
        with type bound = B.t
         and type transition = T.t
         and type program = PM.Program.t)
    (SizeApproximation : ApproximationTypes.SizeApproximationType with type bound = B.t and type rv = PM.RV.t) =
struct
  open PM

  type t = (TransitionApproximation.t, SizeApproximation.t) approximation_t

  let empty =
    {
      time = TransitionApproximation.empty "time";
      size = SizeApproximation.empty;
      cost = TransitionApproximation.empty "cost";
    }


  let time appr = appr.time
  let size appr = appr.size
  let cost appr = appr.cost

  let filter_transitions_and_rvs filter_transitions filter_rvs t =
    {
      time = TransitionApproximation.filter_transitions filter_transitions t.time;
      size = SizeApproximation.filter_rvs filter_rvs t.size;
      cost = TransitionApproximation.filter_transitions filter_transitions t.cost;
    }


  (** Helper methods*)
  let filter_aseq_finite = Sequence.filter ~f:(fun (_, b) -> B.is_finite b)

  (** Sizebound related methods *)

  let sizebound appr t v = SizeApproximation.get (size appr) (t, v)

  let all_finite_sizebounds : t -> (RV.t * B.t) Sequence.t =
    filter_aseq_finite % SizeApproximation.to_sequence % size


  let add_sizebound bound transition var = Lens.size ^%= SizeApproximation.add bound (transition, var)
  let add_sizebounds bound scc = Lens.size ^%= SizeApproximation.add_all bound scc

  let is_size_bounded program appr t =
    not @@ Set.exists ~f:(fun v -> B.is_infinity @@ sizebound appr t v) (Program.input_vars program)


  (** Timebound related methods *)

  let timebound : t -> T.t -> B.t = TransitionApproximation.get % time

  let all_finite_timebounds : t -> (T.t * B.t) Sequence.t =
    filter_aseq_finite % TransitionApproximation.to_sequence % time


  let program_timebound = TransitionApproximation.sum % time
  let add_timebound bound transition = Lens.time ^%= TransitionApproximation.add bound transition
  let all_times_bounded : t -> T.t Sequence.t -> bool = TransitionApproximation.all_bounded % time
  let is_time_bounded appr = not % B.is_infinity % timebound appr

  (** Costbound related methods *)

  let costbound : t -> T.t -> B.t = TransitionApproximation.get % cost

  let all_finite_costbounds : t -> (T.t * B.t) Sequence.t =
    filter_aseq_finite % TransitionApproximation.to_sequence % cost


  let program_costbound = TransitionApproximation.sum % cost
  let add_costbound bound transition = Lens.cost ^%= TransitionApproximation.add bound transition

  let to_formatted ?(show_initial = false) ?(pretty = false) ?(termination_only = false) (program : Program.t)
      appr =
    let approximable_transitions = Sequence.to_list (T.all_from_program program) in

    let overall_timebound = program_timebound appr program in
    mk_str_header_big "All Bounds"
    <>
    if not termination_only then
      (if show_initial then
         mk_paragraph
           (mk_str_header_small "Initial Complexity Problem (after preprocessing)"
           <> Program.to_formatted_string program <> mk_newline)
       else
         FormattedString.Empty)
      <> mk_str_header_small "Timebounds"
      <> mk_paragraph
           (mk_str_line ("Overall timebound:" ^ B.to_string ~pretty overall_timebound)
           <> TransitionApproximation.to_formatted ~pretty approximable_transitions appr.time)
      <> mk_str_header_small "Costbounds"
      <> mk_paragraph
           (mk_str_line ("Overall costbound: " ^ B.to_string ~pretty (program_costbound appr program))
           <> TransitionApproximation.to_formatted ~pretty approximable_transitions appr.cost)
      <> mk_str_header_small "Sizebounds"
      <> mk_paragraph @@ SizeApproximation.to_formatted ~pretty appr.size
    else
      mk_str_header_small "Termination behavior"
      <> mk_paragraph
           (mk_str_line ("Overall termination: " ^ B.to_string ~pretty ~termination_only overall_timebound)
           <> TransitionApproximation.to_formatted ~pretty ~termination_only approximable_transitions
                appr.time)


  (* TODO: use to_formatted *)
  let to_string ?(show_initial = false) ?(pretty = true) ?(termination_only = false) program appr =
    FormattedString.render_string @@ to_formatted ~show_initial:true ~pretty ~termination_only program appr
end

module MakeForClassicalAnalysis (B : BoundType.Bound) (PM : ProgramTypes.ProgramModules) =
  Make (B) (PM) (TransitionApproximation.MakeDefaultApproximableTransition (PM))
    (TransitionApproximation.Make (B) (TransitionApproximation.MakeDefaultApproximableTransition (PM)))
    (SizeApproximation.Make (B) (PM.RV))

module Coerce
    (B : BoundType.Bound)
    (PM : ProgramTypes.ProgramModules)
    (PM' : ProgramTypes.ProgramModules)
    (E : sig
      val trans_eq : (PM.Transition.t, PM'.Transition.t) Type_equal.t
      val rvtuple__eq : (PM.RV.t, PM'.RV.t) Type_equal.t

      val trans_cmp_wit_eq :
        (PM.Transition.comparator_witness, PM'.Transition.comparator_witness) Type_equal.t

      val rvtuple__cmp_wit_eq : (PM.RV.comparator_witness, PM'.RV.comparator_witness) Type_equal.t
    end) =
struct
  let trans_appr_proof =
    let module L = Type_equal.Lift3 (struct
      type ('trans, 'bound, 'trans_cmp_wit) t =
        ('trans, 'bound, 'trans_cmp_wit) TransitionApproximation.transition_approximation_t
    end) in
    L.lift E.trans_eq Type_equal.refl E.trans_cmp_wit_eq


  let size_appr_proof =
    let module L = Type_equal.Lift3 (struct
      type ('rvtuple_, 'bound, 'rvtuple__cmp_wit) t =
        ('rvtuple_, 'bound, 'rvtuple__cmp_wit) SizeApproximation.size_approximation_t
    end) in
    L.lift E.rvtuple__eq Type_equal.refl E.rvtuple__cmp_wit_eq


  let coerce : MakeForClassicalAnalysis(B)(PM).t -> MakeForClassicalAnalysis(B)(PM').t =
   fun appr ->
    Type_equal.
      {
        size = conv size_appr_proof appr.size;
        time = conv trans_appr_proof appr.time;
        cost = conv trans_appr_proof appr.cost;
      }
end

include MakeForClassicalAnalysis (Bounds.Bound) (ProgramModules)

module Probabilistic (BP : BoundPair.T) = struct
  module NonProbOverapprApproximation =
    MakeForClassicalAnalysis (BP.ClassBound) (ProbabilisticProgramModules.NonProbOverappr)

  module ClassicalApproximation = MakeForClassicalAnalysis (BP.ClassBound) (ProbabilisticProgramModules)

  module ExpApproximation =
    Make
      (BP.ProbBound)
      (struct
        include ProbabilisticProgramModules
        module RV = GRV
      end)
      (TransitionApproximation.ApproximableGeneralTransition)
      (TransitionApproximation.Make (BP.ProbBound) (TransitionApproximation.ApproximableGeneralTransition))
      (SizeApproximation.Make (BP.ProbBound) (ProbabilisticProgramModules.GRV))

  type apprs = { appr : ExpApproximation.t; class_appr : ClassicalApproximation.t }

  let coerce_from_nonprob_overappr_approximation : NonProbOverapprApproximation.t -> ClassicalApproximation.t
      =
    let module M =
      Coerce (BP.ClassBound) (ProbabilisticProgramModules.NonProbOverappr) (ProbabilisticProgramModules)
        (ProbabilisticPrograms.Equalities)
    in
    M.coerce


  let coerce_from_classical_approximation : ClassicalApproximation.t -> NonProbOverapprApproximation.t =
    let module M =
      Coerce (BP.ClassBound) (ProbabilisticProgramModules) (ProbabilisticProgramModules.NonProbOverappr)
        (struct
          open Type_equal
          module E = ProbabilisticPrograms.Equalities

          let trans_eq = sym E.trans_eq
          let rvtuple__eq = sym E.rvtuple__eq
          let trans_cmp_wit_eq = sym E.trans_cmp_wit_eq
          let rvtuple__cmp_wit_eq = sym E.rvtuple__cmp_wit_eq
        end)
    in
    M.coerce
end
