open Batteries
   
module MakeMonomial(Var : PolyTypes.ID)(Value : Number.Numeric) =
  struct
    module Valuation_ = Valuation.MakeValuation(Var)(Value)
    module RenameMap_ = RenameMap.MakeRenameMap(Var)
    module Power = Powers.MakePower(Var)(Value)

    type t = Power.t list
    type power = Power.t
           
    module Var = Var
    module Value = Value

    let make powers = powers

    let lift power = [power]
         
    let vars mon = List.unique (List.map Power.var mon)
             
    let degree mon =
         mon
      |> List.map Power.degree
      |> List.fold_left (+) 0

    let degree_variable var mon =
      let var_list = List.filter (fun power -> Var.(==) (Power.var power) var ) mon  in 
      degree var_list  

    let delete_var var mon =
      List.filter(fun x -> let var_x = Power.var x in not (Var.(==) var var_x)) mon
      
    let simplify mon =
         mon
      |> List.group (fun p1 p2 -> Var.compare (Power.var p1) (Power.var p2))
      |> List.map (fun (powers : Power.t list) -> Power.make (Power.var (List.hd powers)) (degree powers))
      |> List.filter (fun (power : Power.t) -> (Power.degree power > 0)) (* Removing ones*)
                  
    let to_string_simplified = function 
        [] -> "1"
      | mon -> String.concat "*" (List.map Power.to_string mon)

    let to_string mon = to_string_simplified (simplify mon)
                      
    let of_string str =
         str
      |> String.split_on_char '*'
      |> List.map Power.of_string
      
    let to_z3_simplified ctx mon = 
      if mon !=[] then Z3.Arithmetic.mk_mul ctx (List.map (Power.to_z3 ctx) mon) 
      else Z3.Arithmetic.Integer.mk_numeral_i ctx 1
      
    let to_z3 ctx mon = 
      to_z3_simplified ctx (simplify mon)

    (*compares two monomials under the assumption that both have already been simplified*)
    let rec equal_simplified mon1 mon2 =
      if (List.length mon1 == List.length mon2) then
        match mon1 with
        | [] -> true (*same length, hence mon2 == []*)
        | pow1::tail1 -> 
           let var1 = Power.var pow1 in
           ((degree_variable var1 mon2) == (Power.degree pow1)) && (equal_simplified tail1 (delete_var var1 mon2))     
      else false
      
    let is_univariate_linear mon =
      match vars mon with
        [x] -> degree_variable x mon == 1
      | _ -> false

    let (==) mon1 mon2 = equal_simplified (simplify mon1)(simplify mon2)
                       
    let rename varmapping mon = 
      List.map (Power.rename varmapping) mon

    (*Multiplication of monomials*)

    let mul mon1 mon2 =
      simplify (List.append mon1 mon2)  

    let eval mon valuation = 
      List.fold_left Value.mul Value.one (List.map (fun power -> Power.eval power valuation) mon)

    let one = []
                    
  end
