open Koat2
open Batteries
open Bounds
open OUnit2
open Formulas
open Constraints
open Atoms
open Polynomials

let assert_equal_bool = assert_equal ~printer:Bool.to_string ~cmp:Bool.equal
let assert_equal_string = assert_equal ~printer:identity ~cmp:String.equal
let assert_equal_int = assert_equal ~printer:string_of_int ~cmp:Int.equal

let assert_equal_varset_enum =
  assert_equal ~cmp:(Enum.equal Base.Set.equal) ~printer:(Util.enum_to_string VarSet.to_string)


let assert_equal_value = assert_equal ~cmp:OurInt.( =~= ) ~printer:OurInt.to_string
let assert_equal_varset = assert_equal ~cmp:Base.Set.equal ~printer:VarSet.to_string
let assert_equal_poly = assert_equal ~cmp:Polynomial.( =~= ) ~printer:Polynomial.to_string

let assert_equal_realpoly_smt =
  let cmp p1 p2 =
    (* Z3Solver is *not* thread-safe compared to the incremental one *)
    let s = SMT.IncrementalZ3Solver.create () in
    SMT.IncrementalZ3Solver.add_real s (RealFormula.mk_uneq p1 p2);
    SMT.IncrementalZ3Solver.unsatisfiable s
  in
  assert_equal ~cmp ~printer:RationalPolynomial.to_string


let assert_equal_parameter_poly =
  assert_equal ~cmp:ParameterPolynomial.( =~= ) ~printer:ParameterPolynomial.to_string


let assert_equal_bound = assert_equal ~cmp:Bound.( =~= ) ~printer:Bound.to_string

let assert_equal_bound_option =
  assert_equal ~cmp:(Option.eq ~eq:Bound.( =~= )) ~printer:(Util.option_to_string Bound.to_string)


let assert_ge_realbound_smt =
  let module Solver = SMT.IncrementalZ3Solver in
  let cmp b1 b2 =
    (* Z3Solver is *not* thread-safe compared to the incremental one *)
    let s = Solver.create () in
    Solver.add_realbound_comparison s `LT b1 b2;
    not (Solver.satisfiable s)
  in
  assert_equal ~cmp ~printer:RealBound.to_string


let assert_equal_atom = assert_equal ~cmp:Atom.( =~= ) ~printer:Atom.to_string
let assert_equal_constr = assert_equal ~cmp:Constraint.( =~= ) ~printer:Constraint.to_string
let convert_lsb b = b |> Option.map LocalSizeBound.as_bound |? Bound.infinity

let assert_equal_formula =
  assert_equal
    ~cmp:(fun f1 f2 ->
      SMT.Z3Solver.unsatisfiable Formula.Infix.((f1 && Formula.neg f2) || (Formula.neg f1 && f2)))
    ~printer:Formula.to_string


let assert_equal_program =
  assert_equal ~cmp:ProgramModules.Program.equivalent ~printer:ProgramModules.Program.to_string


let assert_true = assert_bool ""
let assert_false b = assert_true (not b)
let assert_exception f = assert_true (Result.is_bad (Result.catch f ()))
