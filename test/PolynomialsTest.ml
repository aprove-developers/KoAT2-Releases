open Batteries
open ID
module VarMap = Map.Make(StringID)
module VariableTerm = Variables.MakeVariableTerm(StringID)
module Power = Powers.MakePower(StringID)
module Monomial = Monomials.MakeMonomial(StringID)
module ScaledMonomial = ScaledMonomials.MakeScaledMonomial(StringID)
module Polynomial = Polynomials.MakePolynomial(StringID)
module Valuation = Valuation.MakeValuation(StringID)
open Z3
let () =	
    let x = (VariableTerm.of_string "x") in
    let y = (VariableTerm.of_string "y") in
    let z = (VariableTerm.of_string "z") in
    Printf.printf "x is %s\n" (VariableTerm.to_string x);
        let pow1 = Power.make x 2 in
        let pow2 = Power.make y 3 in
        let pow3 = Power.make z 0 in
            let mon1 = Monomial.make [pow1;pow2;pow1;pow2] in
            let mon2 = Monomial.make [pow2;pow2;pow1;pow1;pow3;pow3;pow3] in
            let const = Monomial.one in
            Printf.printf "Running Z3 version %s\n" Version.to_string ;
            Printf.printf "Z3 full version string: %s\n" Version.full_version ;
            let cfg = [("model", "true"); ("proof", "false")] in
            let ctx = (mk_context cfg) in

            Printf.printf "Monomial mon1 = %s \n" (Monomial.to_string mon1);
            Printf.printf "Simplified Monomial = %s \n" (Monomial.to_string (Monomial.simplify mon1));
            
            Printf.printf "Monomial mon2 = %s \n" (Monomial.to_string mon2);
            Printf.printf "Simplified Monomial mon2 = %s \n"  (Monomial.to_string (Monomial.simplify mon2));

            Printf.printf "EqualityTest = %B \n" (Monomial.(==) mon1 mon2);
            Printf.printf "Degree of mon1 is %d\n" (Monomial.degree mon1);

            Printf.printf "Variables of mon1 are %s\n" (String.concat "," (List.map VariableTerm.to_string (Monomial.vars mon1)));
            Printf.printf "Degree of x in mon1 is %d \n" (Monomial.degree_variable x mon1);
            Printf.printf "Monomial mon1 in Z3 = %s \n" (Z3.Expr.to_string (Monomial.to_z3 ctx mon1));
            
            Printf.printf "Constant Monomial = %s\n" (Monomial.to_string const);
            Printf.printf "Constant Monomial in Z3 = %s \n" (Z3.Expr.to_string (Monomial.to_z3 ctx const));

                let scaled1 = ScaledMonomial.make (Big_int.big_int_of_int 2) mon1 in
                let scaled2 = ScaledMonomial.make (Big_int.big_int_of_int 1) (Monomial.lift pow1) in
                let scaled3 = ScaledMonomial.make (Big_int.big_int_of_int(-1)) (Monomial.lift pow2) in
                let scaled4 = ScaledMonomial.make (Big_int.big_int_of_int (-3)) mon2 in
                let scaled5 = ScaledMonomial.make (Big_int.big_int_of_int 0) mon2 in
                let scaled_const = ScaledMonomial.make (Big_int.big_int_of_int 123) const in
                    Printf.printf "Constant Scaled Monomial = %s \n " (ScaledMonomial.to_string scaled_const);
                    let poly1 = Polynomial.make [scaled1 ; scaled2 ; scaled3 ; scaled4; scaled4 ; scaled5 ; scaled5 ; scaled_const ; scaled5 ; scaled5 ; scaled5] in
                    let poly2 = Polynomial.make [scaled2 ; scaled3 ; scaled4 ; scaled1 ; scaled4 ; scaled5 ; scaled5 ; scaled_const] in

                    Printf.printf "poly1 = %s\n" (Polynomial.to_string poly1);
                    
                    Printf.printf "Coefficient of mon1 in poly1 = %s \n" (Big_int.string_of_big_int (Polynomial.coeff mon1 poly1));
                    Printf.printf "Simplified Polynomial poly1  is = %s\n" (Polynomial.to_string (Polynomial.simplify poly1));
                    Printf.printf "Scaled z3 = %s \n" (Z3.Expr.to_string (Polynomial.to_z3 ctx poly1));
                    Printf.printf "Simplified Polynomial poly2  is = %s\n" (Polynomial.to_string (Polynomial.simplify poly2));
                    Printf.printf "poly1 == poly2 ? %B \n" (Polynomial.(==) poly1 poly2);
                    Printf.printf "zero polynomial = %s \n" (Polynomial.to_string (Polynomial.zero));
                    Printf.printf "constant polynomial 1 = %s \n" (Polynomial.to_string (Polynomial.one));
                    Printf.printf "1 = 0 ? %B \n" (Polynomial.(==) (Polynomial.zero) (Polynomial.one)); 

                    Printf.printf "zero polynomial in Z3 = %s \n" (Z3.Expr.to_string (Polynomial.to_z3 ctx (Polynomial.zero)));

                    Printf.printf "unit polynomial in Z3 = %s \n" (Z3.Expr.to_string (Polynomial.to_z3 ctx (Polynomial.one)));

                    Printf.printf "get_constant unit polynomial = %s\n" (Big_int.string_of_big_int (Polynomial.constant (Polynomial.one)));

                    Printf.printf "get_constant of poly1 = %s\n" (Big_int.string_of_big_int (Polynomial.constant poly1));
                    
                    Printf.printf "get_variables of poly = %s\n" (String.concat ", " (List.map VariableTerm.to_string (Polynomial.vars poly1)));
                    
                    Printf.printf "get_monomials of poly = %s\n" (String.concat "," (List.map (Monomial.to_string) (Polynomial.monomials poly1))) ;

                    Printf.printf "get_degree of poly = %d\n" (Polynomial.degree poly1);
 
                    let poly3 = Polynomial.from_var (VariableTerm.of_string "z") in 
                    
                    Printf.printf "poly3 = %s\n" (Polynomial.to_string poly3);

                    Printf.printf "get_degree of poly3 = %d \n"  (Polynomial.degree poly3); 

                    Printf.printf "is_univariate_and_linear poly3 = %B \n"  (Polynomial.is_univariate_linear poly3);

                    let varmapping = VarMap.empty in
                    let varmapping = VarMap.add (StringID.of_string "x") (StringID.of_string "a") varmapping in
                    let varmapping = VarMap.add (StringID.of_string "y") (StringID.of_string "b") varmapping in
                    let varmapping = VarMap.add (StringID.of_string "z") (StringID.of_string "c") varmapping in  
                       Printf.printf "renaming the variables in mon1 yields %s\n" (Monomial.to_string (Monomial.rename varmapping mon1));

                       Printf.printf "renaming the variables in poly1 yields %s\n" (Polynomial.to_string (Polynomial.rename varmapping poly1));
                       Printf.printf "Adding poly1 and poly2 = %s\n" (Polynomial.to_string (Polynomial.add poly1 poly2));

                       Printf.printf "Subtracting poly1 and poly 2 = %s\n" (Polynomial.to_string (Polynomial.subtract poly1 poly2));
                       Printf.printf "Multiplying poly 1 and poly 3 = %s\n" (Polynomial.to_string (Polynomial.mult poly1 poly3));

                       Printf.printf "Binomial formular = %s\n" (Polynomial.to_string (Polynomial.pow (Polynomial.add (Polynomial.from_var x) (Polynomial.from_var y)) 10));


                       Printf.printf "Multinomial formular = %s\n" (Polynomial.to_string (Polynomial.pow (Polynomial.sum [(Polynomial.from_var x); (Polynomial.from_var y) ;(Polynomial.from_var z)]) 3  ));

                       let intmapping = Valuation.from [(StringID.of_string "x", Big_int.big_int_of_int 2);
                                                        (StringID.of_string "y", Big_int.big_int_of_int 5);
                                                        (StringID.of_string "z", Big_int.big_int_of_int 3)] in
                       Printf.printf "instantiating the variables in mon1 yields %s\n" (Big_int.string_of_big_int (Monomial.eval mon1 intmapping));

                       Printf.printf "instantiating the variables in poly1 yields %s\n" (Big_int.string_of_big_int (Polynomial.eval poly1 intmapping))
                      
;;
