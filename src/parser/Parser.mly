%parameter<G : Parseable.TransitionGraph>

%token	<string>	ID
%token	<int>		UINT
%token			PLUS MINUS TIMES POW
%token			EQUAL GREATERTHAN GREATEREQUAL LESSTHAN LESSEQUAL
%token			LPAR RPAR
%token			EOF EOL
%token                  AND
%token 			ARROW WITH
%token			RULES VAR
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

%type <string * string * (G.Transition_.Constraint_.Atom_.Polynomial_.Var.t list -> G.Transition_.t)> transition

%type <(string * string * (G.Transition_.Constraint_.Atom_.Polynomial_.Var.t list -> G.Transition_.t)) list> transitions

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
	|       vars = variables; list(EOL); trans = transitions
		  { G.from vars (List.map (fun (start, target, t) -> (start, target, t vars)) trans) } ;

transitions :
	|	LPAR RULES EOL* l = separated_nonempty_list(EOL, transition) EOL* RPAR
		  { l } ;

variables :   
	|	LPAR VAR vars = list(ID) RPAR
		  { List.map Poly.Var.of_string vars } ;

transition :
	|	lhs = transition_lhs; ARROW; rhs = transition_rhs; constr = withConstraints
                  { (Tuple2.first lhs, Tuple3.first rhs, G.Transition_.mk (Tuple3.second rhs) (List.map Poly.Var.of_string (Tuple2.second lhs)) (Tuple3.third rhs) constr) } ;

transition_lhs :
	|	start = ID; patterns = delimited(LPAR, separated_list(COMMA, ID), RPAR)
	          { (start, patterns) } ;

transition_rhs :
	|	name = ID; LPAR target = ID; LPAR assignments = separated_list(COMMA, polynomial) RPAR RPAR
	          { (target, name, assignments) } ;

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
