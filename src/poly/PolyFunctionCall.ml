open! OurBase
include Polynomials.PolynomialOverIndeterminate (VarFunctionCall) (OurInt)

let of_function_call = of_indeterminate

let substitute_var_f substitution =
  fold ~const:of_constant
    ~indeterminate:(fun v ->
      if VarFunctionCall.is_function_call v then
        of_function_call v
      else
        substitution @@ VarFunctionCall.to_var v)
    ~plus:add ~times:mul ~pow


let substitute_var_function_call_f substitution =
  fold ~const:of_constant ~indeterminate:substitution ~plus:add ~times:mul ~pow


let has_function_calls = List.exists ~f:VarFunctionCall.is_function_call % indeterminates
let function_call_vars = List.filter ~f:VarFunctionCall.is_function_call % indeterminates

let all_vars p =
  Set.union (VarFunctionCallSet.of_varset @@ vars p) (VarFunctionCallSet.of_list @@ function_call_vars p)


exception Rec_Vars of string

let to_poly p : Polynomials.Polynomial.t =
  if has_function_calls p then
    raise (Rec_Vars "Recursive variables not allowed in classical polynomials.")
  else
    fold ~const:Polynomials.Polynomial.of_constant ~plus:Polynomials.Polynomial.add
      ~times:Polynomials.Polynomial.mul ~pow:Polynomials.Polynomial.pow
      ~indeterminate:(Polynomials.Polynomial.of_var % VarFunctionCall.to_var)
      p


let to_poly_overapprox p : Polynomials.Polynomial.t =
  fold ~const:Polynomials.Polynomial.of_constant ~plus:Polynomials.Polynomial.add
    ~times:Polynomials.Polynomial.mul ~pow:Polynomials.Polynomial.pow
    ~indeterminate:(Polynomials.Polynomial.of_var % VarFunctionCall.to_var)
    p


let of_poly (var_poly : Polynomials.Polynomial.t) : t =
  var_poly |> Polynomials.Polynomial.fold ~const:of_constant ~plus:add ~times:mul ~pow ~indeterminate:of_var


let remove_non_contributors_in_function_calls non_contributors =
  substitute_var_function_call_f (fun x ->
      of_function_call @@ VarFunctionCall.remove_non_contributors non_contributors x)


let max_of_occurring_constants =
  fold ~const:OurInt.abs
    ~indeterminate:(fun _ -> OurInt.one)
    ~plus:OurInt.add ~times:OurInt.mul ~pow:OurInt.pow
