open Batteries

module Valuation = Valuation.Make(OurInt)

let description = "Find solutions for a constraint"

let command = "smt"

type params = {

    constr : string; [@pos 0]
    (** The constraint for which a solution should be found. *)

    solver : [`Z3]; [@enum ["z3", `Z3]] [@default `Z3]
    (** The solver which should be used. *)

  } [@@deriving cmdliner, show]

let run (params: params) =
  let module Z3 = SMT.Z3Opt in
  let solve = match params.solver with
    | `Z3 -> Z3.get_model
  and constr = Readers.read_formula params.constr in
  solve constr
  |> Option.map (fun solution ->
         Enum.iter (fun (var,value) -> print_string (Var.to_string var ^ " -> " ^ OurInt.to_string value ^ "\n")) (Valuation.bindings solution)
       )
  |? print_string "unsatisfiable\n"

