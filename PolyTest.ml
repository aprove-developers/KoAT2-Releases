open Poly
open Z3
let _ =	
    let x = (Variables.mk_var "x") in
    let y = (Variables.mk_var "y") in
    Printf.printf "x is %s\n" (Variables.to_string x);
        let pow1 = Powers.mk_pow_from_var x 2 in
        let pow2 = Powers.mk_pow_from_var y 3 in
            let mon = [pow1;pow2] in
            Printf.printf "Running Z3 version %s\n" Version.to_string ;
            Printf.printf "Z3 full version string: %s\n" Version.full_version ;
            let cfg = [("model", "true"); ("proof", "false")] in
            let ctx = (mk_context cfg) in

            Printf.printf "Monomial = %s \n" (Monomials.to_string mon);
            Printf.printf "MonomialZ3 = %s \n" (Z3.Expr.to_string (Monomials.to_z3 ctx mon));
                let scaled1 = ScaledMonomials.mk_scaledMon_from_mon (Big_int.big_int_of_int 2) mon in
                let scaled2 = ScaledMonomials.mk_scaledMon_from_mon (Big_int.big_int_of_int 1) [pow1] in
                let scaled3 = ScaledMonomials.mk_scaledMon_from_mon (Big_int.big_int_of_int(-1)) [pow2] in
                    let poly = [scaled1 ; scaled2 ; scaled3] in
                    Printf.printf "Polynomial = %s\n" (Polynomials.to_string poly);
                    Printf.printf "Scaled z3 = %s \n" (Z3.Expr.to_string (Polynomials.to_z3 ctx poly))

;;
