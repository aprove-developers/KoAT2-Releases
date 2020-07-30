open Batteries
open BoundsInst

let html_header = ["<!DOCTYPE html>";"<html>";"<head>";"<title> KoAT2 Proof </title>";"</head>"]

let html_body body = ["<body>";"<p>" ^ body ^ "</p>";"</body>";"</html>"]

(** Prints the whole resulting approximation with the expected timebounds to the shell. *)
let print_all_expected_bounds ?(html=false) (program: Program.t) (appr: Approximation.t): unit =
    appr
    |> Approximation.to_string ~html:html program true
    |> print_endline

(** Prints the only the deterministic bounds of the resulting approximation to the shell. *)
let print_all_deterministic_bounds?(html=false) (program: Program.t) (appr: Approximation.t): unit =
    appr
    |> Approximation.to_string ~html:html program false
    |> print_endline


(** Prints the overall expected timebound of the program to the shell. *)
let print_overall_expected_timebound ?(html=false) (program: Program.t) (appr: Approximation.t): unit =
  let heading = if html then "<h3>Overall Timebound:</h3>\n" else "Overall Timebound:\n" in
    let result =
        program
        |> Approximation.program_exptimebound appr
        |> fun b -> ((RealBound.show_complexity_termcomp%RealBound.asymptotic_complexity) b) ^ "\n" , heading ^ (RealBound.show ~complexity:false b)
    in
    if html then
        let html_list = (List.singleton (Tuple2.first result)) @ html_header @ (html_body (Tuple2.second result)) in
            html_list
            |> String.concat "\n"
            |> print_endline
    else
        result
        |> fun t -> ((Tuple2.first t) ^ "\n" ^ (Tuple2.second t))
        |> print_endline

(** Prints the overall expected costbound of the program to the shell. *)
let print_overall_expected_costbound ?(html=false) (program: Program.t) (appr: Approximation.t): unit =
    let heading = if html then "<h3>Overall Timebound:</h3>\n" else "Overall Timebound:\n" in
    let result =
        program
        |> Approximation.program_expcostbound appr
        |> fun b -> ((RealBound.show_complexity_termcomp%RealBound.asymptotic_complexity) b) ^ "\n" , heading ^ (RealBound.show ~complexity:false b)
    in
    if html then
        let html_list = (List.singleton (Tuple2.first result)) @ html_header @ (html_body (Tuple2.second result)) in
            html_list
            |> String.concat "\n"
            |> print_endline
    else
        result
        |> fun t -> ((Tuple2.first t) ^ "\n" ^ (Tuple2.second t))
        |> print_endline

(** Prints the overall timebound of the program to the shell. *)
let print_overall_deterministic_timebound ?(html=false) (program: Program.t) (appr: Approximation.t): unit =
  let heading = if html then "<h3>Overall Timebound:</h3>\n" else "Overall Timebound:\n" in
    let result =
        program
        |> Approximation.program_timebound appr
        |> fun b -> (((Bound.show_complexity_termcomp%Bound.asymptotic_complexity) b) ^ "\n" , heading ^ (Bound.show ~complexity:false b))
    in
    if html then
        let html_list = (List.singleton (Tuple2.first result)) @ html_header @ (html_body (Tuple2.second result)) in
            html_list
            |> String.concat "\n"
            |> print_endline
    else
        result
        |> fun t -> ((Tuple2.first t) ^ "\n" ^ (Tuple2.second t))
        |> print_endline


(** Prints the overall deterministic timebound of the program to the shell. *)
let print_termcomp_deterministic ?(html=false) (program: Program.t) (appr: Approximation.t): unit =
    let result = 
        program
        |> Approximation.program_costbound appr
        |> Bound.asymptotic_complexity
        |> Bound.show_complexity_termcomp
    in
    result
    |> print_endline

(** Prints the overall expected timebound of the program to the shell. *)
let print_termcomp_expected ?(html=false) (program: Program.t) (appr: Approximation.t): unit =
    let result =
        program
        |> Approximation.program_exptimebound appr
        |> RealBound.asymptotic_complexity
        |> RealBound.show_complexity_termcomp
    in
    result
    |> print_endline