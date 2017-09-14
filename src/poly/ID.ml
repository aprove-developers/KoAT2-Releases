open Batteries

(** Implements a variable as a String *)
module StringID =
  struct
    type t =
      | Var of String.t
      | Helper of int
                
    let (=~=) id1 id2 =
      match (id1, id2) with
      | (Var str1, Var str2) -> String.equal str1 str2
      | (Helper i1, Helper i2) -> i1 == i2
      | (_, _) -> false
              
    let of_string str = Var str
    
    let mk_helper n = Helper n
                      
    let to_string = function
      | Var str -> str
      | Helper i -> "_" ^ (String.of_int i)
                  
    let compare id1 id2 =
      match (id1, id2) with
      | (Var str1, Var str2) -> String.compare str1 str2
      | (Helper i1, Helper i2) -> Int.compare i1 i2
      | (Var str, Helper i) -> (-1)
      | (Helper i, Var str) -> 1

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

  end

    (* TODO Add freshvar support if prepost var are used
module PrePostID =
  struct
    type t = Pre of StringID.t | Post of StringID.t
                                       
    let (=~=) a b = match (a, b) with
      | (Pre _, Post _) -> false
      | (Post _, Pre _) -> false
      | (Pre id1, Pre id2) -> id1 == id2
      | (Post id1, Post id2) -> id1 == id2
                              
    let of_string str =
      if String.ends_with str "'" then
        Post (String.rchop str)
      else Pre str
      
    let to_string = function
      | Pre str -> str
      | Post str -> str ^ "'"
                  
    let compare a b = raise (Failure "compare for PrePostID not yet implemented")
  end
   *)
