open OurBase
open BoundsInst
open Formatter
open FormattedString
open Lens.Infix

module Make(B: BoundType.Bound)
           (PM: ProgramTypes.ProgramModules)
           (T: TransitionApproximationType.ApproximableTransition with type program = PM.Program.t) = struct
  open PM
  module TransitionApproximation = TransitionApproximationType.Make(B)(T)
  module SizeApproximation = SizeApproximationType.Make(B)(RV)

  type t = {
      time: TransitionApproximation.t;
      size: SizeApproximation.t;
      cost: TransitionApproximation.t;
    } [@@deriving lens { submodule = true }]

  let empty transitioncount varcount = {
      time = TransitionApproximation.empty "time";
      size = SizeApproximation.empty (2 * transitioncount * varcount);
      cost = TransitionApproximation.empty "cost";
    }

  let create program =
    empty (TransitionGraph.nb_edges (Program.graph program))
          (Set.length (Program.vars program))

  let time appr = appr.time

  let size appr = appr.size

  let cost appr = appr.cost


  (** Sizebound related methods *)

  let sizebound appr t v =
    SizeApproximation.get (size appr) (t,v)

  let add_sizebound bound transition var =
    Lens.size ^%= SizeApproximation.add bound (transition,var)

  let add_sizebounds bound scc =
    Lens.size ^%= SizeApproximation.add_all bound scc

  let is_size_bounded program appr t =
    not @@ Set.exists ~f:(fun v -> B.is_infinity @@ sizebound appr t v ) (Program.input_vars program)

  (** Timebound related methods *)

  let timebound =
    TransitionApproximation.get % time

  let program_timebound =
    TransitionApproximation.sum % time

  let add_timebound bound transition =
    Lens.time ^%= TransitionApproximation.add bound transition

  let all_times_bounded =
    TransitionApproximation.all_bounded % time

  let is_time_bounded appr =
    not % B.is_infinity % timebound appr

  (** Costbound related methods *)

  let costbound =
    TransitionApproximation.get % cost

  let program_costbound =
    TransitionApproximation.sum % cost

  let add_costbound bound transition =
    Lens.cost ^%= TransitionApproximation.add bound transition

  let to_formatted ?(show_initial=false) ?(pretty=false) ?(termination_only=false) (program: Program.t) appr =
    let approximable_transitions = Sequence.to_list (T.all_from_program program) in

    let overall_timebound = program_timebound appr program in
    mk_str_header_big "All Bounds" <>

      (if not termination_only then
          (if show_initial then
             mk_paragraph (
               (mk_str_header_small "Initial Complexity Problem (after preprocessing)"
                <> (Program.to_formatted_string program) <> mk_newline) )
           else FormattedString.Empty )
          
        <> mk_str_header_small "Timebounds" <> ( mk_paragraph (
            mk_str_line ("Overall timebound:" ^ B.to_string ~pretty overall_timebound)
            <> TransitionApproximation.to_formatted ~pretty approximable_transitions appr.time) )
            
        <> mk_str_header_small "Costbounds" <> ( mk_paragraph (
            mk_str_line ("Overall costbound: " ^ B.to_string ~pretty (program_costbound appr program))
            <> TransitionApproximation.to_formatted ~pretty approximable_transitions appr.cost ) ) 
        
        <> mk_str_header_small "Sizebounds" <> (mk_paragraph @@ SizeApproximation.to_formatted ~pretty appr.size)
      else 
        mk_str_header_small "Termination behavior" <> ( mk_paragraph (
          mk_str_line ("Overall termination: " ^ B.to_string ~pretty ~termination_only overall_timebound)
          <> TransitionApproximation.to_formatted ~pretty ~termination_only approximable_transitions appr.time) ))

  (* TODO: use to_formatted *)
  let to_string ?(show_initial=false) ?(pretty=true) ?(termination_only=false) program appr =
    FormattedString.render_string @@ to_formatted ~show_initial:true ~pretty ~termination_only program appr
end

module MakeWithDefaultTransition(B: BoundType.Bound)(PM: ProgramTypes.ProgramModules) =
  Make(B)(PM)(TransitionApproximationType.MakeDefaultApproximableTransition(PM))

module MakeForClassicalAnalysis(PM: ProgramTypes.ProgramModules) =
  MakeWithDefaultTransition(BoundsInst.Bound)(PM)

module Coerce(B: BoundType.Bound)
             (PM: ProgramTypes.ProgramModules)(PM': ProgramTypes.ProgramModules)
             (E: sig
                val trans_proof: (PM.Transition.t,PM'.Transition.t) Type_equal.t
                val rvtuple__proof: (PM.RV.RVTuple_.t,PM'.RV.RVTuple_.t) Type_equal.t
                val trans_cmp_wit_proof: (PM.Transition.comparator_witness,PM'.Transition.comparator_witness) Type_equal.t
              end) = struct
  let trans_appr_proof =
    let module L =
      Type_equal.Lift3(struct
        type ('trans,'bound,'trans_cmp_wit) t =
          ('trans,'bound,'trans_cmp_wit) TransitionApproximationType.transition_approximation_t
      end)
    in
    L.lift E.trans_proof Type_equal.refl E.trans_cmp_wit_proof

  let size_appr_proof =
    let module L = Type_equal.Lift2(struct type ('a,'b) t = ('a,'b) SizeApproximationType.size_approximation_t end) in
    L.lift E.rvtuple__proof Type_equal.refl

  let coerce: MakeWithDefaultTransition(B)(PM).t -> MakeWithDefaultTransition(B)(PM').t = fun appr ->
    Type_equal.{ size = conv size_appr_proof  appr.size
               ; time = conv trans_appr_proof appr.time
               ; cost = conv trans_appr_proof appr.cost
               }
end

include MakeForClassicalAnalysis(ProgramModules)


module Probabilistic = struct
  module NonProbOverapprApproximation = MakeForClassicalAnalysis(ProbabilisticProgramModules.NonProbOverappr)
  module ClassicalApproximation =
    MakeWithDefaultTransition(BoundsInst.Bound)(ProbabilisticProgramModules)
  module ExpApproximation =
    Make(BoundsInst.RealBound)
        (struct
          include ProbabilisticProgramModules
          module RV = GRV
         end)
        (struct
          open ProbabilisticProgramModules
          type program = Program.t
          include GeneralTransition
          let id = gt_id
          let all_from_program = Set.to_sequence % Program.gts
        end)

  let coerce_from_nonprob_overappr_approximation: NonProbOverapprApproximation.t -> ClassicalApproximation.t =
    let module M = Coerce(BoundsInst.Bound)
                         (ProbabilisticProgramModules.NonProbOverappr)(ProbabilisticProgramModules)
        (struct
          let trans_proof = ProbabilisticPrograms.Equalities.trans_eq
          let rvtuple__proof = ProbabilisticPrograms.Equalities.rvtuple__eq
          let trans_cmp_wit_proof = ProbabilisticPrograms.Equalities.trans_cmp_wit_eq
        end)
    in
    M.coerce

end
