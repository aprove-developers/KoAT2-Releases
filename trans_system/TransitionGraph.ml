open Format
open Graph
module TransitionGraph = Imperative.Digraph.AbstractLabeled(Locations)(Transitions)