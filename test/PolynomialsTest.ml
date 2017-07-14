open Batteries
open ID
module VarMap = Map.Make(StringID)
module VariableTerm = StdPoly.VariableTerm
module Power = StdPoly.Power
module Monomial = StdPoly.Monomial
module ScaledMonomial = StdPoly.ScaledMonomial
module Polynomial = StdPoly.Polynomial
module Valuation = StdPoly.Valuation
open Z3
open OUnit2
open PolyTypes
   
let x = VariableTerm.of_string "x"
let y = VariableTerm.of_string "y"
let z = VariableTerm.of_string "z"

let pow1 = Power.make x 2
let pow2 = Power.make y 3
let pow3 = Power.make z 0

let mon1 = Monomial.make [pow1;pow2;pow1;pow2]
let mon2 = Monomial.make [pow2;pow2;pow1;pow1;pow3;pow3;pow3]

let cfg = [("model", "true"); ("proof", "false")]
let ctx = (mk_context cfg)

let scaled1 = ScaledMonomial.make (Big_int.of_int 2) mon1
let scaled2 = ScaledMonomial.make (Big_int.of_int 1) (Monomial.lift pow1)
let scaled3 = ScaledMonomial.make (Big_int.of_int (-1)) (Monomial.lift pow2)
let scaled4 = ScaledMonomial.make (Big_int.of_int (-3)) mon2
let scaled5 = ScaledMonomial.make (Big_int.of_int 0) mon2
let scaled_const = ScaledMonomial.make (Big_int.of_int 123) Monomial.one

let poly1 = Polynomial.make [scaled1 ; scaled2 ; scaled3 ; scaled4; scaled4 ; scaled5 ; scaled5 ; scaled_const ; scaled5 ; scaled5 ; scaled5]
let poly2 = Polynomial.make [scaled2 ; scaled3 ; scaled4 ; scaled1 ; scaled4 ; scaled5 ; scaled5 ; scaled_const]
let poly3 = Polynomial.from_var (VariableTerm.of_string "z")
                    
let varmapping = VarMap.empty
let varmapping = VarMap.add (StringID.of_string "x") (StringID.of_string "a") varmapping
let varmapping = VarMap.add (StringID.of_string "y") (StringID.of_string "b") varmapping
let varmapping = VarMap.add (StringID.of_string "z") (StringID.of_string "c") varmapping

let intmapping = Valuation.from [(StringID.of_string "x", Big_int.big_int_of_int 2);
                                 (StringID.of_string "y", Big_int.big_int_of_int 5);
                                 (StringID.of_string "z", Big_int.big_int_of_int 3)]

module ParserTest(Var : ID) =
  struct
               
    module AST = PolynomialAST(Var)
               
    (* Unambigous transformation to string to make tests more readable *)
    let rec polynomial_to_string (ex : AST.t) = match ex with
      | AST.Constant c -> string_of_int c
      | AST.Variable v -> Var.to_string v
      | AST.Neg t -> String.concat "" ["("; "-"; polynomial_to_string t; ")"]
      | AST.Plus (t1,t2) -> String.concat "" ["("; polynomial_to_string t1; "+"; polynomial_to_string t2; ")"]
      | AST.Times (t1,t2) -> String.concat "" ["("; polynomial_to_string t1; "*"; polynomial_to_string t2; ")"]
      | AST.Pow (t,n) -> String.concat "" ["("; polynomial_to_string t; "^"; string_of_int n; ")"]
                       
    module Parser = PolynomialParser.Make(Var)
    module Lexer = PolynomialLexer.Make(Var)
                 
    let process str =
         str
      |> Lexing.from_string
      |> Parser.polynomial Lexer.read
      |> polynomial_to_string

    let tests =
      "Parser" >::: [
          "Positive Tests" >::: [
            "Constant" >::
              (fun _ -> assert_equal "42" (process " 42 "));
            "Negated Constant" >::
              (fun _ -> assert_equal "(-42)" (process " - 42 "));
            "Variable" >::
              (fun _ -> assert_equal "x" (process " x "));
            "Negated Variable" >::
              (fun _ -> assert_equal "(-x)" (process " - x "));
            "Parentheses" >::
              (fun _ -> assert_equal "x" (process " ( x ) "));
            "Double Parentheses" >::
              (fun _ -> assert_equal "x" (process " ( ( x ) )"));
            "Constant Addition" >::
              (fun _ -> assert_equal "(42+24)" (process " 42 + 24 "));
            "Constant Subtraction" >::
              (fun _ -> assert_equal "(42+(-24))" (process " 42 - 24 "));
            "Constant Multiplication" >::
              (fun _ -> assert_equal "(42*24)" (process " 42 * 24 "));
            "Addition" >::
              (fun _ -> assert_equal "((x+24)+y)" (process " x + 24 + y "));
            "Subtraction" >::
              (fun _ -> assert_equal "((42+(-24))+(-x))" (process " 42 - 24 - x "));
            "Multiplication" >::
              (fun _ -> assert_equal "((42*x)*24)" (process " 42 * x * 24 "));
            "Power" >::
              (fun _ -> assert_equal "(x^3)" (process " x ^ 3 "));
            "Double Negation" >::
              (fun _ -> assert_equal "(-(-42))" (process " - - 42 "));
            "Multiplication before Addition" >::
              (fun _ -> assert_equal "(x+(y*z))" (process " x + y * z "));
            "Power before Negation" >::
              (fun _ -> assert_equal "(-(x^3))" (process " - x ^ 3 "));
            "Power before Multiplication" >::
              (fun _ -> assert_equal "((x^3)*(y^4))" (process " x ^ 3 * y ^ 4 "));
          ];
          "Negative Tests" >::: [
              "Power with negative exponent" >::
                (fun _ -> assert_raises Parser.Error (fun _ -> process " x ^ - 3 "));
              "Power with variable exponent" >::
                (fun _ -> assert_raises Parser.Error (fun _ -> process " x ^ y "));
              "Power with term exponent" >::
                (fun _ -> assert_raises Parser.Error (fun _ -> process " x ^ (y + 3) "));
            ]
        ]

  end

module StringIDParser = ParserTest(StringID)
module PrePostParser = ParserTest(PrePostID)

let suite =
  "Suite" >::: [
      StringIDParser.tests;
      PrePostParser.tests
    ]
                     
let () =
  run_test_tt_main suite
