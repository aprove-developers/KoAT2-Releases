open Batteries
open BoundsInst
open FormatMonad
open FormatMonad.Monad
open Formatter

let doc_title = "KoAT2 Proof"

let print_all_bounds_with_overall_result exp ?(format=Formatter.Plain) ?(overall_expbound=None) (program: Program.t) (appr: Approximation.t): unit =
  let output =
      title doc_title
      >> str (Approximation.overall_result_string ~overall_expbound:overall_expbound program exp appr)
      >> newline
      >> newline
      >> Approximation.output_formatted
        ~embeddings:(if Formatter.is_html format then [FormattedString.mk_raw_str (GraphPrint.get_system_for_paper ~format:"svg" program)] else [])
        program exp appr
  in

  print_string @@
    (if (Formatter.is_html format) then Approximation.overall_result_string program exp appr ^ "\n\n" else "")
    ^ Formatter.render_default ~format:format output

(** Prints the whole resulting approximation with the expected timebounds to the shell. *)
let print_all_expected_bounds: ?format:Formatter.format -> Program.t -> Approximation.t -> unit =
  print_all_bounds_with_overall_result true ~overall_expbound:None

(** Prints the whole resulting approximation with the expected timebounds to the shell. *)
let print_all_expected_bounds_expected_size var ?(format=Formatter.Plain) program appr: unit =
  let overall_expsize_bound = Some (Approximation.program_expsizebound appr program var) in
  print_all_bounds_with_overall_result true ~format:format ~overall_expbound:overall_expsize_bound program appr

(** Prints the only the deterministic bounds of the resulting approximation to the shell. *)
let print_all_deterministic_bounds: ?format:Formatter.format -> Program.t -> Approximation.t -> unit =
  print_all_bounds_with_overall_result false ~overall_expbound:None

(** Prints overall expected bounds of the program to the shell. *)
let print_overall_expected_bound ?(format=Formatter.Plain) bound: unit =
  let r_termcomp = RealBound.show_complexity_termcomp @@ RealBound.asymptotic_complexity bound in
  let r_bound    = str_header_big "Overall Bound:" >> str (RealBound.show ~complexity:false bound) in
  let output_body = title doc_title >> r_bound in
  print_string @@ r_termcomp ^  "\n\n" ^ Formatter.render_default ~format:format output_body

(** Prints the overall expected timebound of the program to the shell. *)
let print_overall_expected_timebound ?(format=Formatter.Plain) (program: Program.t) (appr: Approximation.t): unit =
  let bound = Approximation.program_exptimebound appr program in
  print_overall_expected_bound ~format:format bound

(** Prints the overall expected sizebound of the program for a given variable to the shell. *)
let print_overall_expected_costbound ?(format=Formatter.Plain) (program: Program.t) (appr: Approximation.t): unit =
  let bound = Approximation.program_expcostbound appr program in
  print_overall_expected_bound ~format:format bound

(** Prints the overall expected sizebound of the program for a given variable to the shell. *)
let print_overall_expected_sizebound ?(format=Formatter.Plain) (var: Var.t) (program: Program.t) (appr: Approximation.t): unit =
  let bound = Approximation.program_expsizebound appr program var in
  print_overall_expected_bound ~format:format bound


(** Prints overall expected bounds of the program to the shell. *)
let print_overall_deterministic_timebound ?(format=Formatter.Plain) (program: Program.t) (appr: Approximation.t): unit =
  let bound = Approximation.program_timebound appr program in
  let r_termcomp = Bound.show_complexity_termcomp @@ Bound.asymptotic_complexity bound in
  let r_bound    = str_header_big "Overall Timebound:" >> str (Bound.show ~complexity:false bound) in
  let output_body = title doc_title >> r_bound in
  print_string @@ r_termcomp ^  Formatter.render_default ~format:format output_body


(** Prints the overall deterministic timebound of the program as plaintext to the shell. *)
let print_termcomp_deterministic (program: Program.t) (appr: Approximation.t): unit =
    let result =
        program
        |> Approximation.program_costbound appr
        |> Bound.asymptotic_complexity
        |> Bound.show_complexity_termcomp
    in
    result
    |> print_endline

(** Prints the overall expected timebound of the program as plaintext to the shell. *)
let print_termcomp_expected (program: Program.t) (appr: Approximation.t): unit =
  let result =
    program
    |> Approximation.program_exptimebound appr
    |> RealBound.asymptotic_complexity
    |> RealBound.show_complexity_termcomp
  in
  result
  |> print_endline

(** Prints the overall expected timebound of the program as plaintext to the shell. *)
let print_termcomp_expected_size v (program: Program.t) (appr: Approximation.t): unit =
  let result =
    program
    |> fun p -> Approximation.program_expsizebound appr p v
    |> RealBound.asymptotic_complexity
    |> RealBound.show_complexity_termcomp
  in
  result
  |> print_string
