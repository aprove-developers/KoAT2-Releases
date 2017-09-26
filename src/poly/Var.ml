open Batteries

type t =
  | Var of String.t
  | Helper of int [@@deriving eq, ord]
            
let (=~=) = equal
          
let of_string str = Var str
                  
let mk_helper n = Helper n
                
let to_string = function
  | Var str -> str
  | Helper i -> "_" ^ (String.of_int i)
              
let counter = ref 0
            
let fresh_id () =
  incr counter;
  Helper !counter
  
let fresh_ids n =
  Enum.take n (Enum.from fresh_id)
  
let fresh_id_list n =
  List.of_enum (fresh_ids n)
  
(**returns true if the variable represents real numbers*)
let is_helper var =
  match var with 
  |Var name -> false
  |Helper name -> true
