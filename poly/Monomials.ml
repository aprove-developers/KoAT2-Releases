open Batteries

module MakePower(Var : PolyTypes.ID)(Value : Number.Numeric) =
  struct
    module Valuation_ = Valuation.Make(Var)(Value)
    module RenameMap_ = RenameMap.Make(Var)
                      
    type t = {
        var : Var.t;
        n : int
      }

    module Var = Var
    module Value = Value

    let fold ~const ~var ~times ~pow power =
      pow (var power.var) power.n

    let of_string name = {
        var = Var.of_string name;
        n = 1
      }

    let to_string power =
      if power.n <= 0 then "1"
      else if power.n == 1 then Var.to_string power.var
      else Var.to_string power.var ^ "^" ^ string_of_int power.n

    let (==) power1 power2 =
      (power1.var == power2.var) && ( power1.n == power2.n)

    let vars power = Set.singleton power.var

    let eval power valuation =
      if power.n < 0 then Value.zero
      else Value.pow (Valuation_.eval power.var valuation) (Value.of_int power.n)
               
    let rename varmapping power = {
        power with var = RenameMap_.find power.var varmapping power.var
      }

    let make var n = {
        var = var;
        n = n
      }

    let lift var = make var 1

    let var t = t.var
                   
    let n t = t.n

    let degree = n

  end
  
module Make(Var : PolyTypes.ID)(Value : Number.Numeric) =
  struct
    module Valuation_ = Valuation.Make(Var)(Value)
    module RenameMap_ = RenameMap.Make(Var)
    module Power = MakePower(Var)(Value)

    type t = Power.t list
    type power = Power.t
           
    module Var = Var
    module Value = Value

    let make = List.map (fun (var, n) -> Power.make var n)

    let lift var n = [Power.make var n]

    let fold ~const ~var ~times ~pow =
      List.fold_left (fun b power -> times b (Power.fold ~const ~var ~times ~pow power)) (const Value.one)
                   
    let vars mon = Set.of_list (List.map Power.var mon)
             
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
      if Set.cardinal (vars mon) == 1 then
        degree_variable (Set.choose (vars mon)) mon == 1
      else false

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
