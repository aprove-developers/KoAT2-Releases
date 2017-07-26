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

                  
			
%start <C.atom> polynomialConstraintAtom

%start <C.t> polynomialConstraints

%type <C.PolynomialConstraintsAtoms_.Polynomial_.t> expression

%type <C.atom> atom

%type <C.t> constraints

%%

polynomialConstraints :
        |       constr = constraints EOF {constr}
        
constraints :
        |       constr = separated_list(COMMA, atom)
                {C.mk constr}

polynomialConstraintAtom :
	|	at = atom EOF { at };

atom :
        |   p1 = expression; EQUAL ; p2 = expression
            {C.PolynomialConstraintsAtoms_.mk_eq p1 p2}
        |   p1 = expression; NEQ ; p2 = expression
            {C.PolynomialConstraintsAtoms_.mk_neq p1 p2}
        |   p1 = expression; GREATERTHAN ; p2 = expression
            {C.PolynomialConstraintsAtoms_.mk_gt p1 p2}
        |   p1 = expression; GREATEREQUAL ; p2 = expression
            {C.PolynomialConstraintsAtoms_.mk_ge p1 p2}
        |   p1 = expression; LESSTHAN ; p2 = expression
            {C.PolynomialConstraintsAtoms_.mk_lt p1 p2}
        |   p1 = expression; LESSEQUAL ; p2 = expression
            {C.PolynomialConstraintsAtoms_.mk_le p1 p2}

variable :
	|	v = ID
                  { C.PolynomialConstraintsAtoms_.Polynomial_.from_var_string v }

expression :
	|       v = variable
                  { v }
	| 	c = UINT
                  { C.PolynomialConstraintsAtoms_.Polynomial_.from_constant_int c }
	|	LPAR; ex = expression; RPAR
                  { ex }
	|       MINUS; ex = expression
	          { C.PolynomialConstraintsAtoms_.Polynomial_.neg ex }
	|       ex1 = expression; PLUS; ex2 = expression
	          { C.PolynomialConstraintsAtoms_.Polynomial_.add ex1 ex2 }
	|       ex1 = expression; TIMES; ex2 = expression
	          { C.PolynomialConstraintsAtoms_.Polynomial_.mul ex1 ex2 }
	|       ex1 = expression; MINUS; ex2 = expression
	          { C.PolynomialConstraintsAtoms_.Polynomial_.add ex1 (C.PolynomialConstraintsAtoms_.Polynomial_.neg ex2) }
	|       v = variable; POW; c = UINT
	          { C.PolynomialConstraintsAtoms_.Polynomial_.pow v c } ;
