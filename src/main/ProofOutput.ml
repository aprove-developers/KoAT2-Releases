open Batteries
open BoundsInst
open FormatMonad
open FormatMonad.Monad
open Formatter

let doc_title = "KoAT2 Proof"

let print_all_bounds_with_overall_result exp ?(html=false) (program: Program.t) (appr: Approximation.t): unit =
  let output =
      title doc_title
      >> str (Approximation.overall_result_string program exp appr)
      >> newline
      >> newline
      >> Approximation.output_formatted ~embed_raw_svg:html program exp appr
  in

  print_string @@
    (if html then Approximation.overall_result_string program exp appr ^ "\n\n" else "")
    ^ Formatter.render_default ~format:(if html then Html else Plain) output


(** Prints the whole resulting approximation with the expected timebounds to the shell. *)
let print_all_expected_bounds: ?html:bool -> Program.t -> Approximation.t -> unit =
  print_all_bounds_with_overall_result true

(** Prints the only the deterministic bounds of the resulting approximation to the shell. *)
let print_all_deterministic_bounds: ?html:bool -> Program.t -> Approximation.t -> unit =
  print_all_bounds_with_overall_result false

(** Prints overall expected bounds of the program to the shell. *)
let print_overall_expected_bound ?(html=false) bound: unit =
  let r_termcomp = RealBound.show_complexity_termcomp @@ RealBound.asymptotic_complexity bound in
  let r_bound    = str_header_big "Overall Timebound:" >> str (RealBound.show ~complexity:false bound) in
  let output_body = title doc_title >> r_bound in
  print_string @@ r_termcomp ^  "\n\n" ^ Formatter.render_default ~format:(if html then Html else Plain) output_body

(** Prints the overall expected timebound of the program to the shell. *)
let print_overall_expected_costbound ?(html=false) (program: Program.t) (appr: Approximation.t): unit =
    let bound = Approximation.program_exptimebound appr program in
    print_overall_expected_bound ~html:html bound

(** Prints the overall expected costbound of the program to the shell. *)
let print_overall_expected_costbound ?(html=false) (program: Program.t) (appr: Approximation.t): unit =
    let bound = Approximation.program_expcostbound appr program in
    print_overall_expected_bound ~html:html bound


(** Prints overall expected bounds of the program to the shell. *)
let print_overall_deterministic_timebound ?(html=false) (program: Program.t) (appr: Approximation.t): unit =
  let bound = Approximation.program_timebound appr program in
  let r_termcomp = Bound.show_complexity_termcomp @@ Bound.asymptotic_complexity bound in
  let r_bound    = str_header_big "Overall Timebound:" >> str (Bound.show ~complexity:false bound) in
  let output_body = title doc_title >> r_bound in
  print_string @@ r_termcomp ^  Formatter.render_default ~format:(if html then Html else Plain) output_body


(** Prints the overall deterministic timebound of the program as plaintext to the shell. *)
let print_termcomp_deterministic (program: Program.t) (appr: Approximation.t): unit =
    let result =
        program
        |> Approximation.program_costbound appr
        |> Bound.asymptotic_complexity
        |> Bound.show_complexity_termcomp
    in
    result
    |> print_string

(** Prints the overall expected timebound of the program as plaintext to the shell. *)
let print_termcomp_expected (program: Program.t) (appr: Approximation.t): unit =
    let result =
        program
        |> Approximation.program_exptimebound appr
        |> RealBound.asymptotic_complexity
        |> RealBound.show_complexity_termcomp
    in
    result
    |> print_string