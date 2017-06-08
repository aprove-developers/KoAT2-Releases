open Poly
open Z3
let _ =	
    let x = (Variables.mk_var "x") in
    let y = (Variables.mk_var "y") in
    let z = (Variables.mk_var "z") in
    Printf.printf "x is %s\n" (Variables.to_string x);
        let pow1 = Powers.mk_pow_from_var x 2 in
        let pow2 = Powers.mk_pow_from_var y 3 in
        let pow3 = Powers.mk_pow_from_var z 0 in
            let mon = [pow1;pow2;pow1;pow2] in
            let mon2 = [pow2;pow2;pow1;pow1;pow3;pow3;pow3] in
            Printf.printf "Running Z3 version %s\n" Version.to_string ;
            Printf.printf "Z3 full version string: %s\n" Version.full_version ;
            let cfg = [("model", "true"); ("proof", "false")] in
            let ctx = (mk_context cfg) in

            Printf.printf "Monomial mon = %s \n" (Monomials.to_string mon);
            Printf.printf "Simplified Monomial = %s \n" (Monomials.to_string (Monomials.simplify mon));
            
            Printf.printf "Monomial mon2 = %s \n" (Monomials.to_string mon2);
            Printf.printf "Simplified Monomial mon2 = %s \n"  (Monomials.to_string (Monomials.simplify mon2));

            Printf.printf "EqualityTest = %B \n" (Monomials.equal mon mon2);
            Printf.printf "Degree of mon is %d\n" (Monomials.get_degree mon);

            Printf.printf "Variables of mon are %s\n" (String.concat "," (List.map Variables.to_string (Monomials.get_variables mon)));
            Printf.printf "Degree of x in mon is %d \n" (Monomials.get_degree_variable x mon);
            Printf.printf "Monomial mon in Z3 = %s \n" (Z3.Expr.to_string (Monomials.to_z3 ctx mon));
                let scaled1 = ScaledMonomials.mk_scaledMon_from_mon (Big_int.big_int_of_int 2) mon in
                let scaled2 = ScaledMonomials.mk_scaledMon_from_mon (Big_int.big_int_of_int 1) [pow1] in
                let scaled3 = ScaledMonomials.mk_scaledMon_from_mon (Big_int.big_int_of_int(-1)) [pow2] in
                let scaled4 = ScaledMonomials.mk_scaledMon_from_mon (Big_int.big_int_of_int (-3)) mon2 in
                let scaled5 = ScaledMonomials.mk_scaledMon_from_mon (Big_int.big_int_of_int 0) mon2 in
                    let poly1 = [scaled1 ; scaled2 ; scaled3 ; scaled4; scaled4 ; scaled5 ; scaled5 ; scaled5 ; scaled5 ; scaled5] in
                    let poly2 = [scaled2 ; scaled3 ; scaled4 ; scaled1 ; scaled4 ; scaled5 ; scaled5] in
                    Printf.printf "poly1 = %s\n" (Polynomials.to_string poly1);
                    Printf.printf "Coefficient of mon in poly1 = %s \n" (Big_int.string_of_big_int (Polynomials.get_coeff mon poly1));
                    Printf.printf "Simplified Polynomial poly1  is = %s\n" (Polynomials.to_string (Polynomials.simplify poly1));
                    Printf.printf "Scaled z3 = %s \n" (Z3.Expr.to_string (Polynomials.to_z3 ctx poly1));
                    Printf.printf "Simplified Polynomial poly2  is = %s\n" (Polynomials.to_string (Polynomials.simplify poly2));
                    Printf.printf "poly1 == poly2 ? %B \n" (Polynomials.equal poly1 poly2)

;;
