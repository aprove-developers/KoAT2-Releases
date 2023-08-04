open OurBase

module type LabelPrint = sig
  type label
  val print_label: label -> string
end

module type GraphPrint = sig
  type location
  type transition_label
  type transition = location * transition_label * location
  type transition_comparator_witness
  type program

  type color = Black | Red | Blue | Green | Yellow | Purple | Brown | White [@@deriving ord, eq]

  val empty_color_map: (transition,color,transition_comparator_witness) Map.t

  (** Prints a png file in the given directory with the given filename (the extension .png will be generated) for the transition graph of the program.
          For this operation graphviz need to be installed and the 'dot' command must be accessible in the PATH. *)
  val print_system: label:(location * transition_label * location -> string) -> outdir:Fpath.t -> file:string -> program -> format:string -> unit

  val print_system_pretty: ?file_format:string -> ?color_map:(transition,color,transition_comparator_witness) Map.t -> program -> string option

  val print_system_pretty_html:  (transition,color,transition_comparator_witness) Map.t -> program -> string
end
