open! OurBase

module PolyRec = struct
  include Polynomials.PolynomialOverIndeterminate (VarRec) (OurInt)

  let of_varrec = of_indeterminate

  let substitute_var_f substitution =
    fold ~const:of_constant
      ~indeterminate:(fun v ->
        if VarRec.is_rec v then
          of_varrec v
        else
          substitution @@ VarRec.to_var v)
      ~plus:add ~times:mul ~pow


  let substitute_varrec_f substitution =
    fold ~const:of_constant ~indeterminate:substitution ~plus:add ~times:mul ~pow


  let has_recvars = List.exists ~f:VarRec.is_rec % indeterminates
  let rec_vars = List.filter ~f:VarRec.is_rec % indeterminates
  let all_vars p = Set.union (VarRecSet.of_varset @@ vars p) (VarRecSet.of_list @@ rec_vars p)

  exception Rec_Vars of string

  let to_poly p : Polynomials.Polynomial.t =
    if has_recvars p then
      raise (Rec_Vars "Recursive variables not allowed in classical polynomials.")
    else
      fold ~const:Polynomials.Polynomial.of_constant ~plus:Polynomials.Polynomial.add
        ~times:Polynomials.Polynomial.mul ~pow:Polynomials.Polynomial.pow
        ~indeterminate:(Polynomials.Polynomial.of_var % VarRec.to_var)
        p


  let to_poly_overapprox p : Polynomials.Polynomial.t =
    fold ~const:Polynomials.Polynomial.of_constant ~plus:Polynomials.Polynomial.add
      ~times:Polynomials.Polynomial.mul ~pow:Polynomials.Polynomial.pow
      ~indeterminate:(Polynomials.Polynomial.of_var % VarRec.to_var_or_tmp)
      p


  let of_poly (var_poly : Polynomials.Polynomial.t) : t =
    var_poly |> Polynomials.Polynomial.fold ~const:of_constant ~plus:add ~times:mul ~pow ~indeterminate:of_var


  let remove_non_contributors_in_rec_vars non_contributors =
    substitute_varrec_f (fun x -> of_varrec @@ VarRec.remove_non_contributors non_contributors x)


  let max_of_occurring_constants =
    fold ~const:OurInt.abs
      ~indeterminate:(fun _ -> OurInt.one)
      ~plus:OurInt.add ~times:OurInt.mul ~pow:OurInt.pow
end
