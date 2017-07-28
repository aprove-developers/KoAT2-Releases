%parameter<C : ConstraintTypes.ParseablePolynomialConstraints>

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

%start <C.PolynomialConstraintsAtoms_.t> polynomialConstraintAtom

%start <C.PolynomialConstraintsAtoms_.Polynomial_.t> polynomial

%type <C.PolynomialConstraintsAtoms_.Polynomial_.t> expression

%type <C.PolynomialConstraintsAtoms_.t> atom

%type <C.t> constraints

%{
  module Atom = C.PolynomialConstraintsAtoms_
  module Poly = C.PolynomialConstraintsAtoms_.Polynomial_
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
        |   p1 = expression; EQUAL ; p2 = expression
            {Atom.mk_eq p1 p2}
        |   p1 = expression; NEQ ; p2 = expression
            {Atom.mk_neq p1 p2}
        |   p1 = expression; GREATERTHAN ; p2 = expression
            {Atom.mk_gt p1 p2}
        |   p1 = expression; GREATEREQUAL ; p2 = expression
            {Atom.mk_ge p1 p2}
        |   p1 = expression; LESSTHAN ; p2 = expression
            {Atom.mk_lt p1 p2}
        |   p1 = expression; LESSEQUAL ; p2 = expression
            {Atom.mk_le p1 p2}

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
	|       ex1 = expression; PLUS; ex2 = expression
	          { Poly.add ex1 ex2 }
	|       ex1 = expression; TIMES; ex2 = expression
	          { Poly.mul ex1 ex2 }
	|       ex1 = expression; MINUS; ex2 = expression
	          { Poly.add ex1 (Poly.neg ex2) }
	|       v = variable; POW; c = UINT
	          { Poly.pow v c } ;
