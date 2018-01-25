open Batteries
open ProgramTypes

module RV =
  struct
    type t = Transition.t * Var.t

    let same (t1,v1) (t2,v2) =
      Transition.same t1 t2
      && Var.equal v1 v2

    let equivalent (t1,v1) (t2,v2) =
      Transition.equivalent t1 t2
      && Var.equal v1 v2

    let compare compare_transition (t1,v1) (t2,v2) =
      if compare_transition t1 t2 != 0 then
        compare_transition t1 t2
      else if Var.compare v1 v2 != 0 then
        Var.compare v1 v2
      else
        0

    let compare_same =
      compare Transition.compare_same
      
    let compare_equivalent =
      compare Transition.compare_equivalent

    let hash (t,v) =
      Hashtbl.hash (Transition.to_string t ^ Var.to_string v)
      
    let transition (t,_) = t
                         
    let variable (_,v) = v
                       
    let to_id_string (t,v) =
      "|" ^ Transition.to_id_string t ^ "," ^ Var.to_string v ^ "|"

    let to_string get_lsb kind ((l,t,l'), v) =
      let comp = function
        | `Lower -> "<="
        | `Upper -> ">="
      in
      String.concat " " [Bound.to_string (get_lsb kind t v);
                         comp kind;
                         to_id_string ((l,t,l'), v)]
  end

module RVG =
  struct
    include Graph.Persistent.Digraph.ConcreteBidirectional(struct
                include RV
                let equal = same
                let compare = compare_same
              end)

    type scc = RV.t list

    let rvs_to_id_string rvs =
      rvs
      |> List.map RV.to_id_string
      |> String.concat ","

    let rvs_to_string get_lsb rvs =
      rvs
      |> List.map (fun rv -> RV.to_string get_lsb `Lower rv ^ ", " ^ RV.to_string get_lsb `Upper rv)
      |> String.concat ","

    let pre rvg rv =
      pred rvg rv
      |> List.enum

    (* TODO Optimizable *)
    let entry_points rvg scc =
      scc
      |> List.enum
      |> Enum.map (pre rvg)
      |> Enum.flatten
      |> Enum.uniq_by RV.same
      |> Util.intersection RV.same (List.enum scc)

    let transitions scc =
      scc
      |> List.enum
      |> Enum.map RV.transition
      |> Enum.uniq_by Transition.same
  end  
