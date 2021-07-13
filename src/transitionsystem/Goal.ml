type probabilistic_koat_goal = ExpectedComplexity
                             | ExpectedSize of Var.t

type goal = Complexity
          | ExactRuntime
          | ProbabilisticGoal of probabilistic_koat_goal
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
  | Unknown ->
      "Unknown"
