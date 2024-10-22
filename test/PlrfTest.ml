open OUnit2
open Koat2
open! OurBase
open Polynomials

let tests =
  "PlrfTests"
  >::: List.map
         ~f:(fun (name, loc, rank_exp, prog_dir) ->
           name >:: fun _ ->
           let program = Readers.read_probabilistic_program (prog_dir ^ name ^ ".koat") in
           let plrfs = Plrf.find program in
           match Sequence.hd plrfs with
           | None -> assert_failure "No Plrf found for program!"
           | Some plrf ->
               let rank = Plrf.rank plrf loc in
               let error_msg =
                 "Rank " ^ RationalPolynomial.to_string rank ^ " is not equal to expected rank "
                 ^ RationalPolynomial.to_string rank_exp
               in
               assert_equal rank_exp rank ~cmp:RationalPolynomial.equal ~msg:error_msg)
         [
           ( "variable_prob",
             Location.of_string "g",
             RationalPolynomial.var "Arg_0",
             "../../../examples/probabilistic/" );
           ( "variable_prob2",
             Location.of_string "g",
             RationalPolynomial.var "Arg_0",
             "../../../examples/probabilistic/" );
         ]
