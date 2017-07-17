open Batteries
open ID
module VarMap = Map.Make(StringID)
module Power = StdPoly.Power
module Monomial = StdPoly.Monomial
module ScaledMonomial = StdPoly.ScaledMonomial
module Polynomial = StdPoly.Polynomial
module Valuation = StdPoly.Valuation
open Z3
let () =	
    let x = (StringID.of_string "x") in
    let y = (StringID.of_string "y") in
    Printf.printf "x is %s\n" (StringID.to_string x);
        let pow1 = Power.make x 2 in
        let pow2 = Power.make y 3 in
            let mon1 = Monomial.lift pow1 in
            let mon2 = Monomial.lift pow2 in
            let const = Monomial.one in
            Printf.printf "Running Z3 version %s\n" Version.to_string ;
            Printf.printf "Z3 full version string: %s\n" Version.full_version ;
            let cfg = [("model", "true"); ("proof", "false")] in
            let ctx = (mk_context cfg) in
                let scaled1 = ScaledMonomial.make (Big_int.big_int_of_int 2) mon1 in
                let scaled2 = ScaledMonomial.make (Big_int.big_int_of_int 1) (Monomial.lift pow1) in
                let scaled3 = ScaledMonomial.make (Big_int.big_int_of_int(-1)) (Monomial.lift pow2) in
                let scaled4 = ScaledMonomial.make (Big_int.big_int_of_int (-3)) mon2 in
                let scaled5 = ScaledMonomial.make (Big_int.big_int_of_int 0) mon2 in
                let scaled_const = ScaledMonomial.make (Big_int.big_int_of_int 123) const in
                    let poly1 = Polynomial.make [scaled1 ; scaled2 ; scaled3 ; scaled4; scaled4 ; scaled5 ; scaled5 ; scaled_const ; scaled5 ; scaled5 ; scaled5] in
                    let poly2 = Polynomial.make [scaled2 ; scaled3 ; scaled4 ; scaled1 ; scaled4 ; scaled5 ; scaled5 ; scaled_const] in
                    let poly3 = Polynomial.from_var (StringID.of_string "z") in 
                    
                    let greater_equal = PolynomialConstraintsAtoms.mk_ge poly1 poly2 in
                    let equal = PolynomialConstraintsAtoms.mk_eq poly2 poly3 in
                    
                    Printf.printf "poly 1 >= poly 2 : %s \n" (PolynomialConstraintsAtoms.to_string greater_equal);
                    Printf.printf "poly 1 >= poly 2 in Z3 :\n %s \n" (Z3.Expr.to_string (PolynomialConstraintsAtoms.to_z3 ctx greater_equal));
                    Printf.printf "poly 2 = poly 3 : %s \n" (PolynomialConstraintsAtoms.to_string equal);
                    Printf.printf "poly 2 = poly3 in Z3 :\n %s \n" (Z3.Expr.to_string (PolynomialConstraintsAtoms.to_z3 ctx equal));
                    Printf.printf "The variables in equal are : %s \n" (String.concat ", " (List.map StringID.to_string (PolynomialConstraintsAtoms.get_variables equal)));
                    
                    let varmapping = VarMap.empty in
                    let varmapping = VarMap.add (StringID.of_string "x") (StringID.of_string "a") varmapping in
                    let varmapping = VarMap.add (StringID.of_string "y") (StringID.of_string "b") varmapping in
                    let varmapping = VarMap.add (StringID.of_string "z") (StringID.of_string "c") varmapping in  
                        let greater_equal_ren = PolynomialConstraintsAtoms.rename_vars varmapping greater_equal in
                        let equal_ren = PolynomialConstraintsAtoms.rename_vars varmapping equal in
                        Printf.printf "poly 1 >= poly 2 : %s \n" (PolynomialConstraintsAtoms.to_string greater_equal_ren);
                        Printf.printf "poly 1 >= poly 2 in Z3 :\n %s \n" (Z3.Expr.to_string (PolynomialConstraintsAtoms.to_z3 ctx greater_equal_ren));
                        Printf.printf "poly 2 = poly 3 : %s \n" (PolynomialConstraintsAtoms.to_string equal_ren);
                        Printf.printf "poly 2 = poly3 in Z3 :\n %s \n" (Z3.Expr.to_string (PolynomialConstraintsAtoms.to_z3 ctx equal_ren));
                        Printf.printf "The variables in equal_ren are : %s \n" (String.concat ", " (List.map StringID.to_string (PolynomialConstraintsAtoms.get_variables equal_ren)));
                    
                       let intmapping = Valuation.from [(StringID.of_string "x", Big_int.big_int_of_int 2);
                                                        (StringID.of_string "y", Big_int.big_int_of_int 5);
                                                        (StringID.of_string "z", Big_int.big_int_of_int 3)] in        
                            let greater_equal_in = PolynomialConstraintsAtoms.instantiate_with_big_int intmapping greater_equal in
                            let equal_in = PolynomialConstraintsAtoms.instantiate_with_big_int intmapping equal in
                            Printf.printf "poly1 evaluates to : %s\n" (Big_int.string_of_big_int (Polynomial.eval poly1 intmapping)); 
                            Printf.printf "poly2 evaluates to : %s\n" (Big_int.string_of_big_int (Polynomial.eval poly2 intmapping)); 
                            Printf.printf "poly3 evaluates to : %s\n" (Big_int.string_of_big_int (Polynomial.eval poly3 intmapping)); 
                            Printf.printf "poly 1 >= poly 2 : %B \n" (greater_equal_in);
                            Printf.printf "poly 2 = poly 3 : %B \n" (equal_in);
                            Printf.printf "comparison of constraints : %B \n" (PolynomialConstraintsAtoms.equal greater_equal equal)
;;
