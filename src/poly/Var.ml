open Batteries

type t =
  | Var of String.t
  | Helper of int [@@deriving eq, ord]
            
let (=~=) = equal
          
let of_string str =
    let is_helper_regexp = Str.regexp "[$][_][0-9]+" in
      let is_helper = Str.string_match is_helper_regexp str 0 in
        if is_helper then
          let number_string = String.lchop (String.lchop str) in
            let number = Int.of_string number_string in
             Helper number
        else
          Var str

let hash = Hashtbl.hash
      
let mk_helper n = Helper n
                
let to_string = function
  | Var str -> str
  | Helper i -> "$_" ^ (String.of_int i)
              
let counter = ref 0

(* TODO Use unique from batteries because of thread safety *)
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
