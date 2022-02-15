
open Batteries
open Yices2.High
open Polynomials
open Formulas

module EEH = Make(ExceptionsErrorHandling)

exception SMTFailure of string

let from_poly vars =
  let open EEH in
  Polynomial.fold
    ~const:(fun c -> if OurInt.is_zero c then Term.Arith.zero () else Term.Arith.int (OurInt.to_int c))
    ~var:(fun v -> List.find (fun (v_tmp,v_term) -> String.equal (Var.to_string v_tmp) (Var.to_string v)) vars |> Tuple2.second)
    ~neg:(Term.Arith.neg)
    ~plus:(Term.Arith.add)
    ~times:(Term.Arith.mul)
    (* Somehow Z3.Arithmetic.mk_power makes Z3 use real arithmetic.. *)
    ~pow:(fun b e -> if e = 1 then b else if e > 1 then Term.Arith.(b ^^ e) else failwith "Polynomial exponents should be between 1 and n")

let from_formula vars =
  let open EEH in
  Formula.fold
    ~subject:(from_poly vars)
    ~le:(Term.Arith.leq)
    ~lt:(Term.Arith.lt)
    ~correct:(Term.bool true)
    ~conj:(Term.and2)
    ~wrong:(Term.bool false)
    ~disj:(Term.or2)

module Yices2Solver =
  struct
    module Valuation = Valuation.Make(OurInt)

    let result_is expected_result formula =
      let open EEH in
      let open Global in
        Global.init();
        let vars = List.map (fun v -> v, Term.new_uninterpreted ~name:(Var.to_string v) (if Var.is_real v then Type.real() else Type.int())) (formula |> Formula.vars |> VarSet.to_list) in
        let config = Config.malloc () in
        let _ = Config.default config ~logic:"QF_NIA" in
        Config.set config ~name:"mode" ~value:"one-shot"; (* one opportunity to seize everything you ever wanted. *)
        let context = Context.malloc ~config () in
        let formula_ = from_formula vars formula  in
        Context.assert_formula context formula_;
        let result = Context.check context in
        Config.free config;
        Global.exit();
      if result == Yices2.Low.Types.(`STATUS_UNKNOWN) || result == Yices2.Low.Types.(`STATUS_ERROR) then
        raise (SMTFailure ("SMT-Solver does not know a solution"))
      else
        result == expected_result

    let satisfiable formula =
      result_is Yices2.Low.Types.(`STATUS_SAT) formula

    let unsatisfiable formula =
      result_is Yices2.Low.Types.(`STATUS_UNSAT) formula

    let tautology =
      unsatisfiable % Formula.neg

    let equivalent formula1 formula2 =
      (* Negating formula1 <=> formula2 *)
      Formula.Infix.((formula1 && Formula.neg formula2) || (formula2 && Formula.neg formula1))
      |> unsatisfiable

    (** Returns true iff the formula implies the positivity of the polynomial*)
    let check_positivity (formula : Formula.t) (poly: Polynomial.t) =
      tautology Formula.Infix.(formula => (poly >= Polynomial.zero))

    (** Returns true iff the formula implies the negativity of the polynomial*)
    let check_negativity (formula : Formula.t) (poly: Polynomial.t) =
      tautology Formula.Infix.(formula => (poly <= Polynomial.zero))

    (* Minimise not supported yet. *)
    let get_model ?(coeffs_to_minimise=[]) formula =
      let open EEH in
      let open Global in
        Global.init();
        let vars = List.map (fun v -> v, Term.new_uninterpreted ~name:(Var.to_string v) (if Var.is_real v then Type.real() else Type.int())) (formula |> Formula.vars |> VarSet.to_list) in
        let config = Config.malloc () in
        let _ = Config.default config ~logic:"QF_NIA" in
        Config.set config ~name:"mode" ~value:"one-shot";
        let context = Context.malloc ~config () in
        let formula_ = from_formula vars formula  in
        Context.assert_formula context formula_;
        let result = Context.check context in
        if result == Yices2.Low.Types.(`STATUS_SAT) then
          let model  = Context.get_model context in
          List.map (Tuple2.map2 (OurInt.of_int % Signed.SInt.to_int % (Yices2.Ext_bindings.Model.get_int32_value model))) vars
          |> Valuation.from
          |> Option.some
        else None
  end

