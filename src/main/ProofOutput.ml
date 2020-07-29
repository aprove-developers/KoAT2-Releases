open Batteries
open BoundsInst

let html_header = ["<!DOCTYPE html>";"<html>";"<head>";"<title> KoAT2 Proof </title>";"</head>"]

let html_body body = ["<body>";"<p>" ^ body ^ "</p>";"</body>";"</html>"]

(** Prints the whole resulting approximation with the expected timebounds to the shell. *)
let print_all_expected_bounds ?(html=false) (program: Program.t) (appr: Approximation.t): unit =
    let result =  (Approximation.to_string ~html:html program true appr) in
    if html then
        let html_list = List.append html_header (html_body result) in
            html_list
            |> String.concat "\n"
            |> print_endline
    else
        result
        |> print_endline

(** Prints the only the deterministic bounds of the resulting approximation to the shell. *)
let print_all_deterministic_bounds?(html=false) (program: Program.t) (appr: Approximation.t): unit =
    let result =  (Approximation.to_string ~html:html program false appr) in
    if html then
        let html_list = List.append html_header (html_body result) in
            html_list
            |> String.concat "\n"
            |> print_endline
    else
        result
        |> print_endline


(** Prints the overall expected timebound of the program to the shell. *)
let print_overall_expected_timebound ?(html=false) (program: Program.t) (appr: Approximation.t): unit =
  let heading = if html then "<h3>Overall Timebound:</h3>\n" else "Overall Timebound:\n" in
    let result =
        program
        |> Approximation.program_exptimebound appr
        |> fun b -> ((RealBound.show_complexity_termcomp%RealBound.asymptotic_complexity) b) ^ "\n\n" ^ heading ^ (RealBound.show ~complexity:false b)
    in
    if html then
        let html_list = List.append html_header (html_body result) in
            html_list
            |> String.concat "\n"
            |> print_endline
    else
        result
        |> print_endline

(** Prints the overall expected costbound of the program to the shell. *)
let print_overall_expected_costbound ?(html=false) (program: Program.t) (appr: Approximation.t): unit =
    let heading = if html then "<h3>Overall Timebound:</h3>\n" else "Overall Timebound:\n" in
    let result =
        program
        |> Approximation.program_expcostbound appr
        |> fun b -> ((RealBound.show_complexity_termcomp%RealBound.asymptotic_complexity) b) ^ "\n\n" ^ heading ^ (RealBound.show ~complexity:false b)
    in
    if html then
        let html_list = List.append html_header (html_body result) in
            html_list
            |> String.concat "\n"
            |> print_endline
    else
        result
        |> print_endline

(** Prints the overall timebound of the program to the shell. *)
let print_overall_deterministic_timebound ?(html=false) (program: Program.t) (appr: Approximation.t): unit =
  let heading = if html then "<h3>Overall Timebound:</h3>\n" else "Overall Timebound:\n" in
    let result =
        program
        |> Approximation.program_timebound appr
        |> fun b -> ((Bound.show_complexity_termcomp%Bound.asymptotic_complexity) b) ^ "\n\n" ^ heading ^ (Bound.show ~complexity:false b)
    in
    if html then
        let html_list = List.append html_header (html_body result) in
            html_list
            |> String.concat "\n"
            |> print_endline
    else
        result
        |> print_endline


(** Prints the overall deterministic timebound of the program to the shell. *)
let print_termcomp_deterministic ?(html=false) (program: Program.t) (appr: Approximation.t): unit =
    let result = 
        program
        |> Approximation.program_costbound appr
        |> Bound.asymptotic_complexity
        |> Bound.show_complexity_termcomp
    in
    if html then
        let html_list = List.append html_header (html_body result) in
            html_list
            |> String.concat "\n"
            |> print_endline
    else
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
    if html then
        let html_list = List.append html_header (html_body result) in
            html_list
            |> String.concat "\n"
            |> print_endline
    else
        result
        |> print_endline