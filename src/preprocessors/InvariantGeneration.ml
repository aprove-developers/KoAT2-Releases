open Batteries
open Program.Types
open Polynomials
open Atoms
open Constraints
open PolyTypes

let to_apron_var: Var.t -> Apron.Var.t =
  Apron.Var.of_string % Var.to_string

(** Converts the integer to an apron integer *)
(** Usage of OurInt.to_int breaks usage of big_int *)
let to_apron_const: OurInt.t -> Apron.Coeff.t =
  Apron.Coeff.s_of_int % OurInt.to_int
  
let transform_program program =

  (** Creates the apron environment where all program variables are integer variables. *)
  let environment: Apron.Environment.t =
    Apron.Environment.make (VarSet.map_to_array to_apron_var (Program.vars program)) [||]
  in

  let to_apron_expr (poly: Polynomial.t): Apron.Texpr1.t =
    (* TODO Texpr1.Up: Round in which direction? *) 
    let open Apron in 
    Polynomial.fold
      ~const:(fun c ->
        Texpr1.Cst (to_apron_const c)
      )
      ~var:(fun v ->
        Texpr1.Var (to_apron_var v)
      )
      ~neg:(fun expr ->
        Texpr1.Unop (Texpr1.Neg, expr, Texpr1.Int, Texpr1.Up)
      )
      ~plus:(fun expr1 expr2 ->
        Texpr1.Binop (Texpr1.Add, expr1, expr2, Texpr1.Int, Texpr1.Up)
      )
      ~times:(fun expr1 expr2 ->
        Texpr1.Binop (Texpr1.Mul, expr1, expr2, Texpr1.Int, Texpr1.Up)
      )
      ~pow:(fun expr n ->
        Enum.repeat ~times:n expr
        |> Enum.fold
             (fun result expr -> Texpr1.Binop (Texpr1.Mul, result, expr, Texpr1.Int, Texpr1.Up))
             (Texpr1.Cst (to_apron_const OurInt.one))
      )
      poly
    |> Apron.Texpr1.of_expr environment
  in

  let to_apron_single_constraint (atom: Atom.t): Apron.Tcons1.t =
    let open Apron in
    Atom.fold
      ~subject:to_apron_expr
      (* We have a<=b, Apron uses form c>=0, therefore we need 0<=b-a *)
      ~le:(fun expr1 expr2 -> Tcons1.make (Texpr1.binop Texpr1.Sub expr2 expr1 Texpr1.Int Texpr1.Up) Tcons1.SUPEQ)
      atom
  in

  let to_apron_constraint (constr: Constraint.t): Apron.Tcons1.t array =
    let open Apron in
    constr
    |> Constraint.atom_list
    |> List.map to_apron_single_constraint
    |> Array.of_list
  in
    
  (* TODO Implement *)
  MaybeChanged.same program
