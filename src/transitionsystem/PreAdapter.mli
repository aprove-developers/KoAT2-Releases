module PreAdapterNonRec
    (TL : ProgramTypes.DefaultTransitionLabel)
    (T :
      ProgramTypes.Transition
        with type transition_label = TL.t
         and type transition_label_comparator_witness = TL.comparator_witness)
    (G :
      ProgramTypes.TransitionGraph
        with type transition_label = TL.t
         and type transition_label_comparator_witness = TL.comparator_witness) : sig
  include
    ProgramTypes.PreAdapter
      with type transition_label = TL.t
       and type transition_label_comparator_witness = TL.comparator_witness
       and type transition = Location.t * TL.t * Location.t
       and type transition_comparator_witness = T.comparator_witness
       and type transition_graph = G.t
end

module PreAdapter : sig
  include
    ProgramTypes.PreAdapter
      with type transition_label = TransitionLabel_.t
       and type transition_label_comparator_witness = TransitionLabel_.comparator_witness
       and type transition = Location.t * TransitionLabel_.t * Location.t
       and type transition_comparator_witness = Transition_.MakeClassical(TransitionLabel_).comparator_witness
       and type transition_graph = TransitionGraph_.t
end
