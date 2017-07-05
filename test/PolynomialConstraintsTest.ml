open Mapping
open Z3
let () =	
    let x = (Variables.mk_var "x") in
    let y = (Variables.mk_var "y") in
    let z = (Variables.mk_var "z") in
    Printf.printf "x is %s\n" (Variables.to_string x);
        let pow1 = Powers.mk_pow_from_var x 2 in
        let pow2 = Powers.mk_pow_from_var y 3 in
        let pow3 = Powers.mk_pow_from_var z 0 in
            let mon1 = [pow1] in
            let mon2 = [pow2] in
            let const = [] in
            Printf.printf "Running Z3 version %s\n" Version.to_string ;
            Printf.printf "Z3 full version string: %s\n" Version.full_version ;
            let cfg = [("model", "true"); ("proof", "false")] in
            let ctx = (mk_context cfg) in
                let scaled1 = ScaledMonomials.mk_scaled_mon_from_mon (Big_int.big_int_of_int 2) mon1 in
                let scaled2 = ScaledMonomials.mk_scaled_mon_from_mon (Big_int.big_int_of_int 1) [pow1] in
                let scaled3 = ScaledMonomials.mk_scaled_mon_from_mon (Big_int.big_int_of_int(-1)) [pow2] in
                let scaled4 = ScaledMonomials.mk_scaled_mon_from_mon (Big_int.big_int_of_int (-3)) mon2 in
                let scaled5 = ScaledMonomials.mk_scaled_mon_from_mon (Big_int.big_int_of_int 0) mon2 in
                let scaled_const = ScaledMonomials.mk_scaled_mon_from_mon (Big_int.big_int_of_int 123) const in
                    let poly1 = [scaled1 ; scaled2 ; scaled3 ; scaled4; scaled4 ; scaled5 ; scaled5 ; scaled_const ; scaled5 ; scaled5 ; scaled5] in
                    let poly2 = [scaled2 ; scaled3 ; scaled4 ; scaled1 ; scaled4 ; scaled5 ; scaled5 ; scaled_const] in
                    let poly3 = Polynomials.from_var "z" in 
                    
                    let greater_equal = PolynomialConstraintsAtoms.mk_ge poly1 poly2 in
                    let equal = PolynomialConstraintsAtoms.mk_eq poly2 poly3 in
                    
                    Printf.printf "poly 1 >= poly 2 : %s \n" (PolynomialConstraintsAtoms.to_string greater_equal);
                    Printf.printf "poly 1 >= poly 2 in Z3 :\n %s \n" (Z3.Expr.to_string (PolynomialConstraintsAtoms.to_z3 ctx greater_equal));
                    Printf.printf "poly 2 = poly 3 : %s \n" (PolynomialConstraintsAtoms.to_string equal);
                    Printf.printf "poly 2 = poly3 in Z3 :\n %s \n" (Z3.Expr.to_string (PolynomialConstraintsAtoms.to_z3 ctx equal));
                    Printf.printf "The variables in equal are : %s \n" (Variables.varlist_to_string (PolynomialConstraintsAtoms.get_variables equal));
                    
                    let varmapping = VarMap.empty in
                    let varmapping = VarMap.add "x" "a" varmapping in
                    let varmapping = VarMap.add "y" "b" varmapping in
                    let varmapping = VarMap.add "z" "c" varmapping in 
                        let greater_equal_ren = PolynomialConstraintsAtoms.rename_vars varmapping greater_equal in
                        let equal_ren = PolynomialConstraintsAtoms.rename_vars varmapping equal in
                        Printf.printf "poly 1 >= poly 2 : %s \n" (PolynomialConstraintsAtoms.to_string greater_equal_ren);
                        Printf.printf "poly 1 >= poly 2 in Z3 :\n %s \n" (Z3.Expr.to_string (PolynomialConstraintsAtoms.to_z3 ctx greater_equal_ren));
                        Printf.printf "poly 2 = poly 3 : %s \n" (PolynomialConstraintsAtoms.to_string equal_ren);
                        Printf.printf "poly 2 = poly3 in Z3 :\n %s \n" (Z3.Expr.to_string (PolynomialConstraintsAtoms.to_z3 ctx equal_ren));
                        Printf.printf "The variables in equal_ren are : %s \n" (Variables.varlist_to_string (PolynomialConstraintsAtoms.get_variables equal_ren));
                    
                        let intmapping = VarMap.empty in
                        let intmapping = VarMap.add "x" (Big_int.big_int_of_int 2) intmapping in
                        let intmapping = VarMap.add "y" (Big_int.big_int_of_int 5) intmapping in
                        let intmapping = VarMap.add "z" (Big_int.big_int_of_int 3) intmapping in 
        
                            let greater_equal_in = PolynomialConstraintsAtoms.instantiate_with_big_int intmapping greater_equal in
                            let equal_in = PolynomialConstraintsAtoms.instantiate_with_big_int intmapping equal in
                            Printf.printf "poly1 evaluates to : %s\n" (Big_int.string_of_big_int (Polynomials.eval intmapping poly1)); 
                            Printf.printf "poly2 evaluates to : %s\n" (Big_int.string_of_big_int (Polynomials.eval intmapping poly2)); 
                            Printf.printf "poly3 evaluates to : %s\n" (Big_int.string_of_big_int (Polynomials.eval intmapping poly3)); 
                            Printf.printf "poly 1 >= poly 2 : %B \n" (greater_equal_in);
                            Printf.printf "poly 2 = poly 3 : %B \n" (equal_in);
                            Printf.printf "comparison of constraints : %B \n" (PolynomialConstraintsAtoms.equal greater_equal equal)
;;
