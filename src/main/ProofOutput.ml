open Batteries

let html_header = ["<!DOCTYPE html>";"<html>";"<head>";"<title> KoAT2 Proof </title>";"</head>"]

let html_body body = ["<body>";"<p>" ^ body ^ "</p>";"</body>";"</html>"]

(** Prints the whole resulting approximation to the shell. *)
let print_all_bounds ?(html=false) (program: Program.t) (appr: Approximation.t): unit =
  let result = (Approximation.to_string ~html:html program appr)
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
let print_overall_timebound ?(html=false) (program: Program.t) (appr: Approximation.t): unit =
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

(** Prints only the overall asymptotic timebound of the program to the shell. *)
let print_termcomp ?(html=false) (program: Program.t) (appr: Approximation.t): unit =
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
