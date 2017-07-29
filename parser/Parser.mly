%parameter<C : ConstraintTypes.ParseableConstraint>

%token	<string>	ID
%token	<int>		UINT
%token			PLUS MINUS TIMES POW
%token			EQUAL 
%token			NEQ 
%token			GREATERTHAN 
%token			GREATEREQUAL 
%token			LESSTHAN 
%token			LESSEQUAL
%token			LPAR RPAR
%token			EOF
%token                  AND
%token                  COMMA

%left                   EQUAL NEQ LESSTHAN LESSEQUAL GREATERTHAN GREATEREQUAL
%left			PLUS MINUS
%left			TIMES
%left			POW
                  
			
%start <C.t> polynomialConstraints

%start <C.Atom_.t> polynomialConstraintAtom

%start <C.Atom_.Polynomial_.t> polynomial

%type <C.Atom_.Polynomial_.t> expression

%type <C.Atom_.t> atom

%type <C.t> constraints

%{
  module Atom = C.Atom_
  module Poly = C.Atom_.Polynomial_
%}

%%

polynomialConstraints :
        |       constr = constraints EOF {constr}
        
constraints :
        |       constr = separated_list(COMMA, atom)
                {C.mk constr}

polynomialConstraintAtom :
	|	at = atom EOF { at };
	
polynomial :
        |       poly = expression EOF { poly }

atom :
        |   	p1 = expression; comp = comparator; p2 = expression
                  { comp p1 p2 }

%inline comparator :
  	| 	EQUAL { Atom.mk_eq }
  	| 	NEQ { Atom.mk_neq }
  	| 	GREATERTHAN { Atom.mk_gt }
  	| 	GREATEREQUAL { Atom.mk_ge }
  	| 	LESSTHAN { Atom.mk_lt }
  	| 	LESSEQUAL { Atom.mk_le }

variable :
	|	v = ID
                  { Poly.from_var_string v }

expression :
	|       v = variable
                  { v }
	| 	c = UINT
                  { Poly.from_constant_int c }
	|	LPAR; ex = expression; RPAR
                  { ex }
	|       MINUS; ex = expression
	          { Poly.neg ex }
	|       ex1 = expression; op = bioperator; ex2 = expression
	          { op ex1 ex2 }
	|       v = variable; POW; c = UINT
	          { Poly.pow v c } ;

%inline bioperator :
	|	PLUS { Poly.add }
	|	TIMES { Poly.mul }
	|       MINUS { fun ex1 ex2 -> Poly.add ex1 (Poly.neg ex2) }
