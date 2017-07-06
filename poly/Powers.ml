module VariableTerm = Variables.StringVariableTerm

type var = VariableTerm.t
type valuation = VariableTerm.valuation

type t = 
    {
        var : var; 
        n : int
    }
         
type value = VariableTerm.value

let to_string power =
    if power.n<=0 then "1"
    else if power.n == 1 then (VariableTerm.to_string power.var) 
        else String.concat "^" [(VariableTerm.to_string power.var); (string_of_int power.n)]

let to_z3 ctx power =
    Z3.Arithmetic.mk_power ctx ( VariableTerm.to_z3 ctx power.var ) (Z3.Arithmetic.Integer.mk_numeral_i ctx power.n)
    
let mk_pow_from_var base exponent = 
    {
        var = base;
        n = exponent
    }

let mk_var name =
    {
        var = VariableTerm.of_string name;
        n = 1
    }

let get_variable power = power.var

let get_degree power = power.n

let equal power1 power2 =
    (power1.var == power2.var) && ( power1.n == power2.n)

let rename_power varmapping power =
  mk_pow_from_var (VariableTerm.rename varmapping power.var) power.n
    
let eval valuation power =
    if power.n < 0 then Big_int.zero_big_int
    else Big_int.power_big_int_positive_int (VariableTerm.eval power.var valuation) power.n
