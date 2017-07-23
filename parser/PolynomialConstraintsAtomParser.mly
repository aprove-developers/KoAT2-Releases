%parameter<C : ConstraintTypes.ParseablePolynomialConstraintsAtom>

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
			
%left                   EQUAL NEQ LESSTHAN LESSEQUAL GREATERTHAN GREATEREQUAL
%left			PLUS MINUS
%left			TIMES
%left			POW

                  
			
%start <C.t> polynomialConstraintAtom

%type <C.Polynomial_.t> expression

%type <C.t> atom

%%

polynomialConstraintAtom :
	|	at = atom EOF { at };

atom :
        |   p1 = expression; EQUAL ; p2 = expression
            {C.mk_eq p1 p2}
        |   p1 = expression; NEQ ; p2 = expression
            {C.mk_neq p1 p2}
        |   p1 = expression; GREATERTHAN ; p2 = expression
            {C.mk_gt p1 p2}
        |   p1 = expression; GREATEREQUAL ; p2 = expression
            {C.mk_ge p1 p2}
        |   p1 = expression; LESSTHAN ; p2 = expression
            {C.mk_lt p1 p2}
        |   p1 = expression; LESSEQUAL ; p2 = expression
            {C.mk_le p1 p2}

variable :
	|	v = ID
                  { C.Polynomial_.from_var_string v }

expression :
	|       v = variable
                  { v }
	| 	c = UINT
                  { C.Polynomial_.from_constant_int c }
	|	LPAR; ex = expression; RPAR
                  { ex }
	|       MINUS; ex = expression
	          { C.Polynomial_.neg ex }
	|       ex1 = expression; PLUS; ex2 = expression
	          { C.Polynomial_.add ex1 ex2 }
	|       ex1 = expression; TIMES; ex2 = expression
	          { C.Polynomial_.mul ex1 ex2 }
	|       ex1 = expression; MINUS; ex2 = expression
	          { C.Polynomial_.add ex1 (C.Polynomial_.neg ex2) }
	|       v = variable; POW; c = UINT
	          { C.Polynomial_.pow v c } ;
