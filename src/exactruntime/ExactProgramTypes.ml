open Batteries
open Polynomials
open OurNum

exception Invalid_input of String.t
let list_string vec = 
  vec
  |> List.map OurInt.to_string
  |> String.concat ", "

let make_string str = "\"" ^ str ^ "\""

module ProbUpdate =
  struct
    type t = {
      probability: OurNum.t;
      update: OurInt.t list;
    }
    let from prob up = 
      {
        probability = prob;
        update = up;
      }

    let probability u =
      u.probability
    
    let update u =
      u.update

    let to_string pu =
      OurNum.to_string pu.probability ^ ":(" ^ list_string pu.update ^ ")"
  end

module ExactProgram =
  struct
    type t = {
      guardvector: OurInt.t list;
      guardvalue: OurInt.t;
      updates: ProbUpdate.t list;
      directtermination: ProbUpdate.t option;
      precision: OurInt.t option;
      initial: (OurInt.t list) option;
    }
    let from guardvec guardval up dirterm prec initial =
      {
        guardvector = guardvec;
        guardvalue = guardval;
        updates = up;
        directtermination = dirterm;
        precision = prec;
        initial = initial;
      }
      let guardvector ep =
        ep.guardvector

      let guardvalue ep =
        ep.guardvalue
      
      let updates ep =
        ep.updates
      
      let directtermination ep =
        ep.directtermination
      
      let precision ep =
        ep.precision

      let initial ep =
        ep.initial
      
      let updates_to_string ups =
        ups
        |> List.map ProbUpdate.to_string
        |> String.concat " :+: "
      
      let to_string ep =
        let guard_vec_str = "(GUARDVEC " ^ list_string ep.guardvector ^ ")\n" in
        let guard_val_str = "(GUARDVEC " ^ OurInt.to_string ep.guardvalue ^ ")\n" in
        let update_str = "(UPDATES " ^ updates_to_string ep.updates ^ ")\n" in
        let dir_term_str = Option.map (fun dterm -> "(DIRECTTERMINATION " ^ ProbUpdate.to_string dterm ^ ")\n") (directtermination ep) in
        let prec_str = Option.map (fun prec -> "(PRECISION " ^ OurInt.to_string prec ^ ")\n") (precision ep) in
        let init_str = Option.map (fun init -> "(INITIAL " ^ list_string init ^ ")\n") (initial ep ) in
        guard_vec_str ^ guard_val_str ^ update_str ^ (dir_term_str |? "") ^ (prec_str |? "") ^ (init_str |? "")
      
      let is_valid ?logger ep =
        let report_error str =
          Option.map (fun logger -> Logger.log logger Logger.DEBUG (fun () -> str, [])) logger |> ignore;
          raise (Invalid_input str);
        in
        let prob_greater_zero = 
          ((ep |> directtermination |> Option.map ProbUpdate.probability |> Option.map (Num.(<=/) OurNum.zero)) |? true) &&
          (ep |> updates |> List.map ProbUpdate.probability |> List.for_all (Num.(<=/) OurNum.zero))
          |> tap (fun res ->  if Bool.neg res then
                                report_error "Not all Probabilities are greater than zero.")
        in
        let equals_one =
          OurNum.(+) 
          ((ep |> directtermination |> Option.map ProbUpdate.probability) |? OurNum.zero)
          (ep |> updates |> List.map ProbUpdate.probability |> List.fold_left OurNum.(+) OurNum.zero)
          |> OurNum.equal OurNum.one
          |> tap (fun res ->  if Bool.neg res then report_error "Probabilities do not add up to one.")
        in
        let same_vec_len =
          let update_len =
            ep |> updates |> List.first |> ProbUpdate.update |> List.length
          in
          let same_update_len =
          ep |> updates |> List.map ProbUpdate.update |> List.map List.length
          |> List.for_all (Int.equal update_len)
          |> tap (fun res ->  if Bool.neg res then
                                report_error "Not all update vectors have the same length.")
          in
          let same_term_len = 
            ep |> directtermination 
            |> Option.map (fun dterm -> dterm |> ProbUpdate.update |> List.length)
            |> Option.map_default (Int.equal update_len) true
            |> tap (fun res ->  if Bool.neg res then
                                report_error "Termination vector has the wrong length.")
          in
          (* TODO: add check for guard vec length and initial value *)
          same_update_len && same_term_len
        in
        prob_greater_zero && equals_one && same_vec_len
      
      let to_sage ep =
        let positional = 
          [
            "(" ^ (ep |> guardvector |> list_string) ^ ")";
            ep |> guardvalue |> OurInt.to_string;
            ep |> updates |> updates_to_string
          ]
          |> List.map make_string
        in
        let optional =
          [
            ep |> directtermination |> Option.map ProbUpdate.to_string |> Option.map make_string |> Option.map (fun str -> "--dirterm " ^ str);
            ep |> precision |> Option.map OurInt.to_string |> Option.map make_string |> Option.map (fun str -> "--prec " ^ str);
            ep |> initial |> Option.map list_string |> Option.map (fun str -> "--init \"(" ^ str ^ ")\"")
          ]
          |> List.filter Option.is_some
          |> List.map Option.get
        in
        positional @ optional
        |> String.concat " "
        
  end