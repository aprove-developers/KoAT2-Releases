type probabilistic_koat_goal = ExpectedComplexity
                             | ExpectedSize of Var.t

type goal = Complexity
          | ExactRuntime
          | ProbabilisticGoal of probabilistic_koat_goal
          | ASTermination (* used by LexRSM *)
          | Unknown (* Used by read_simple *)


let supported_analyse_goals = ["COMPLEXITY"; "EXPECTEDCOMPLEXITY"; "EXACTRUNTIME"; "EXPECTEDSIZE"]

let to_string = function
  | Complexity ->
      "COMPLEXITY"
  | ExactRuntime ->
      "EXACTRUNTIME"
  | ProbabilisticGoal ExpectedComplexity ->
      "EXACTRUNTIME"
  | ProbabilisticGoal (ExpectedSize v) ->
      "EXPECTEDSIZE " ^ Var.to_string v
  | ASTermination ->
      "ASTermination"
  | Unknown ->
      "Unknown"
