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
            let mon1 = [pow1;pow2;pow1;pow2] in
            let mon2 = [pow2;pow2;pow1;pow1;pow3;pow3;pow3] in
            let const = [] in
            Printf.printf "Running Z3 version %s\n" Version.to_string ;
            Printf.printf "Z3 full version string: %s\n" Version.full_version ;
            let cfg = [("model", "true"); ("proof", "false")] in
            let ctx = (mk_context cfg) in

            Printf.printf "Monomial mon1 = %s \n" (Monomials.to_string mon1);
            Printf.printf "Simplified Monomial = %s \n" (Monomials.to_string (Monomials.simplify mon1));
            
            Printf.printf "Monomial mon2 = %s \n" (Monomials.to_string mon2);
            Printf.printf "Simplified Monomial mon2 = %s \n"  (Monomials.to_string (Monomials.simplify mon2));

            Printf.printf "EqualityTest = %B \n" (Monomials.equal mon1 mon2);
            Printf.printf "Degree of mon1 is %d\n" (Monomials.get_degree mon1);

            Printf.printf "Variables of mon1 are %s\n" (String.concat "," (List.map Variables.to_string (Monomials.get_variables mon1)));
            Printf.printf "Degree of x in mon1 is %d \n" (Monomials.get_degree_variable x mon1);
            Printf.printf "Monomial mon1 in Z3 = %s \n" (Z3.Expr.to_string (Monomials.to_z3 ctx mon1));
            
            Printf.printf "Constant Monomial = %s\n" (Monomials.to_string const);
            Printf.printf "Constant Monomial in Z3 = %s \n" (Z3.Expr.to_string (Monomials.to_z3 ctx const));

                let scaled1 = ScaledMonomials.mk_scaled_mon_from_mon (Big_int.big_int_of_int 2) mon1 in
                let scaled2 = ScaledMonomials.mk_scaled_mon_from_mon (Big_int.big_int_of_int 1) [pow1] in
                let scaled3 = ScaledMonomials.mk_scaled_mon_from_mon (Big_int.big_int_of_int(-1)) [pow2] in
                let scaled4 = ScaledMonomials.mk_scaled_mon_from_mon (Big_int.big_int_of_int (-3)) mon2 in
                let scaled5 = ScaledMonomials.mk_scaled_mon_from_mon (Big_int.big_int_of_int 0) mon2 in
                let scaled_const = ScaledMonomials.mk_scaled_mon_from_mon (Big_int.big_int_of_int 123) const in
                    Printf.printf "Constant Scaled Monomial = %s \n " (ScaledMonomials.to_string scaled_const);
                    let poly1 = [scaled1 ; scaled2 ; scaled3 ; scaled4; scaled4 ; scaled5 ; scaled5 ; scaled_const ; scaled5 ; scaled5 ; scaled5] in
                    let poly2 = [scaled2 ; scaled3 ; scaled4 ; scaled1 ; scaled4 ; scaled5 ; scaled5 ; scaled_const] in

                    Printf.printf "poly1 = %s\n" (Polynomials.to_string poly1);
                    
                    Printf.printf "Coefficient of mon1 in poly1 = %s \n" (Big_int.string_of_big_int (Polynomials.get_coeff mon1 poly1));
                    Printf.printf "Simplified Polynomial poly1  is = %s\n" (Polynomials.to_string (Polynomials.simplify poly1));
                    Printf.printf "Scaled z3 = %s \n" (Z3.Expr.to_string (Polynomials.to_z3 ctx poly1));
                    Printf.printf "Simplified Polynomial poly2  is = %s\n" (Polynomials.to_string (Polynomials.simplify poly2));
                    Printf.printf "poly1 == poly2 ? %B \n" (Polynomials.equal poly1 poly2);
                    Printf.printf "zero polynomial = %s \n" (Polynomials.to_string (Polynomials.zero));
                    Printf.printf "constant polynomial 1 = %s \n" (Polynomials.to_string (Polynomials.one));
                    Printf.printf "1 = 0 ? %B \n" (Polynomials.equal (Polynomials.zero) (Polynomials.one)); 

                    Printf.printf "zero polynomial in Z3 = %s \n" (Z3.Expr.to_string (Polynomials.to_z3 ctx (Polynomials.zero)));

                    Printf.printf "unit polynomial in Z3 = %s \n" (Z3.Expr.to_string (Polynomials.to_z3 ctx (Polynomials.one)));

                    Printf.printf "get_constant unit polynomial = %s\n" (Big_int.string_of_big_int (Polynomials.get_constant (Polynomials.one)));

                    Printf.printf "get_constant of poly1 = %s\n" (Big_int.string_of_big_int (Polynomials.get_constant poly1));
                    
                    Printf.printf "get_variables of poly = %s\n" (Variables.varlist_to_string (Polynomials.get_variables poly1));
                    
                    Printf.printf "get_monomials of poly = %s\n" (String.concat "," (List.map (Monomials.to_string) (Polynomials.get_monomials poly1))) ;

                    Printf.printf "get_degree of poly = %d\n" (Polynomials.get_degree poly1);
 
                    let poly3 = Polynomials.from_var "z" in 
                    
                    Printf.printf "poly3 = %s\n" (Polynomials.to_string poly3);

                    Printf.printf "get_degree of poly3 = %d \n"  (Polynomials.get_degree poly3); 

                    Printf.printf "is_univariate_and_linear poly3 = %B \n"  (Polynomials.is_univariate_and_linear poly3); 

                   let varmapping = VarMap.empty in
                   let varmapping = VarMap.add "x" "a" varmapping in
                   let varmapping = VarMap.add "y" "b" varmapping in
                   let varmapping = VarMap.add "z" "c" varmapping in  
                       Printf.printf "renaming the variables in mon1 yields %s\n" (Monomials.to_string (Monomials.rename_monomial varmapping mon1));

                       Printf.printf "renaming the variables in poly1 yields %s\n" (Polynomials.to_string (Polynomials.rename_vars varmapping poly1));
                       Printf.printf "Adding poly1 and poly2 = %s\n" (Polynomials.to_string (Polynomials.add poly1 poly2));

                       Printf.printf "Subtracting poly1 and poly 2 = %s\n" (Polynomials.to_string (Polynomials.subtract poly1 poly2));
                       Printf.printf "Multiplying poly 1 and poly 3 = %s\n" (Polynomials.to_string (Polynomials.mult poly1 poly3));

                       Printf.printf "Binomial formular = %s\n" (Polynomials.to_string (Polynomials.pow_poly (Polynomials.add (Polynomials.from_var x) (Polynomials.from_var y)) 10));


                       Printf.printf "Multinomial formular = %s\n" (Polynomials.to_string (Polynomials.pow_poly (Polynomials.add_list [(Polynomials.from_var x); (Polynomials.from_var y) ;(Polynomials.from_var z)]) 3  ));

                   let intmapping = VarMap.empty in
                   let intmapping = VarMap.add "x" (Big_int.big_int_of_int 2) intmapping in
                   let intmapping = VarMap.add "y" (Big_int.big_int_of_int 5) intmapping in
                   let intmapping = VarMap.add "z" (Big_int.big_int_of_int 3) intmapping in  
                       Printf.printf "instantiating the variables in mon1 yields %s\n" (Big_int.string_of_big_int (Monomials.eval intmapping mon1));

                       Printf.printf "instantiating the variables in poly1 yields %s\n" (Big_int.string_of_big_int (Polynomials.eval intmapping poly1))
                      
;;
