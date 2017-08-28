%parameter<G : Parseable.TransitionGraph>

%token	<string>	ID
%token	<int>		UINT
%token			PLUS MINUS TIMES POW
%token			EQUAL GREATERTHAN GREATEREQUAL LESSTHAN LESSEQUAL
%token			LPAR RPAR
%token			EOF
%token                  AND
%token 			ARROW WITH
%token			GOAL STARTTERM FUNCTIONSYMBOLS RULES VAR 
%token                  COMMA

%left                   EQUAL LESSTHAN LESSEQUAL GREATERTHAN GREATEREQUAL
%left			PLUS MINUS
%left			TIMES
%left			POW
                  
			
%start <G.t> onlyTransitiongraph

%start <G.Transition_.Constraint_.t> onlyConstraints

%start <G.Transition_.Constraint_.Atom_.t> onlyAtom

%start <G.Transition_.Constraint_.Atom_.Polynomial_.t> onlyPolynomial

%type <G.t> transitiongraph

%type <G.Transition_.Constraint_.t> constraints

%type <G.Transition_.Constraint_.t> atom

%type <G.Transition_.Constraint_.Atom_.Polynomial_.t> polynomial

%type <G.Transition_.Constraint_.Atom_.Polynomial_.Var.t list> variables

%type <vars:G.Transition_.Constraint_.Atom_.Polynomial_.Var.t list -> G.Transition_.t> transition

%type <(vars:G.Transition_.Constraint_.Atom_.Polynomial_.Var.t list -> G.Transition_.t) list> transitions

%{
  open BatTuple
  module Constr = G.Transition_.Constraint_
  module Atom = G.Transition_.Constraint_.Atom_
  module Poly = G.Transition_.Constraint_.Atom_.Polynomial_
%}

%%

onlyTransitiongraph :
        |       graph = transitiongraph; EOF
                  { graph } ;

transitiongraph :
        |       goal
                start = start
                vars = variables
                trans = transitions
                  { G.from vars (List.map (fun t -> t ~vars) trans) start } ;

goal :		
	|	LPAR GOAL goal = ID RPAR
                  { goal } ;

start :
	|	LPAR STARTTERM LPAR FUNCTIONSYMBOLS start = ID RPAR RPAR
		  { G.Location_.of_string start } ;

transitions :
	|	LPAR RULES l = nonempty_list(transition) RPAR
		  { l } ;

variables :   
	|	LPAR VAR vars = list(ID) RPAR
		  { List.map Poly.Var.of_string vars } ;

transition :
	|	lhs = transition_lhs; ARROW; rhs = transition_rhs; constr = withConstraints
	          { G.Transition_.mk ~name:(Tuple2.first rhs)
                                     ~start:(Tuple2.first lhs)
                                     ~targets:(Tuple2.second rhs)
                                     ~patterns:(List.map Poly.Var.of_string (Tuple2.second lhs))
                                     ~guard:constr } ;

transition_lhs :
	|	start = ID; patterns = delimited(LPAR, separated_list(COMMA, ID), RPAR)
	          { (start, patterns) } ;

transition_rhs :
	|	name = ID; LPAR targets = separated_nonempty_list(COMMA, transition_target) RPAR
 	          { (name, targets) } ;

transition_target :
	|       target = ID; LPAR assignments = separated_list(COMMA, polynomial) RPAR
	          { (target, assignments) } ;

withConstraints :
	|	{ Constr.mk [] }
	|       WITH constr = separated_nonempty_list(AND, atom) { Constr.all constr } ;

onlyConstraints :
        |       constr = constraints EOF { constr } ;
        
constraints :
        |       constr = separated_list(AND, atom)
                  { Constr.all constr } ;

onlyAtom :
	|	p1 = polynomial; comp = atomComparator; p2 = polynomial; EOF
                  { comp p1 p2 } ;

%inline atomComparator :
  	| 	GREATERTHAN { Atom.mk_gt }
  	| 	GREATEREQUAL { Atom.mk_ge }
  	| 	LESSTHAN { Atom.mk_lt }
  	| 	LESSEQUAL { Atom.mk_le } ;
              
atom :
        |   	p1 = polynomial; comp = comparator; p2 = polynomial
                  { comp p1 p2 } ;
                
%inline comparator :
  	| 	EQUAL { Constr.mk_eq }
  	| 	GREATERTHAN { Constr.mk_gt }
  	| 	GREATEREQUAL { Constr.mk_ge }
  	| 	LESSTHAN { Constr.mk_lt }
  	| 	LESSEQUAL { Constr.mk_le } ;

              
onlyPolynomial :
        |       poly = polynomial EOF { poly } ;

variable :
	|	v = ID
                  { Poly.from_var_string v } ;

polynomial :
	|       v = variable
                  { v }
	| 	c = UINT
                  { Poly.from_constant_int c }
	|	LPAR; ex = polynomial; RPAR
                  { ex }
	|       MINUS; ex = polynomial
	          { Poly.neg ex }
	|       p1 = polynomial; op = bioperator; p2 = polynomial
	          { op p1 p2 }
	|       v = variable; POW; c = UINT
	          { Poly.pow v c } ;

%inline bioperator :
	|	PLUS { Poly.add }
	|	TIMES { Poly.mul }
	|       MINUS { fun ex1 ex2 -> Poly.add ex1 (Poly.neg ex2) } ;
