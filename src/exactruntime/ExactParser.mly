%token  <string>	FLOAT
%token  <string>	FRACTION
%token  <string>	VAR
%token	<int>		INT
%token				SUM LISTSUM PROD LISTPROD POW
%token				COS SIN
%token				LPAR RPAR
%token              COMMA
%token				EOF

%start <ExactBound.t> from_tree


%{
  open ExactBound
  open Polynomials
%}

%%


from_tree :
    |	e = expression_bound; EOF
		{ e }

expression_bound :
	|	c = const
		{ ExactBound.const c }
	|	v = var
		{ ExactBound.var v }
	|	LPAR;o = list_op; operands = separated_nonempty_list(COMMA, expression_bound); RPAR
		{ o operands }
	|	LPAR; POW ; base = const; COMMA; exp = expression_bound; RPAR
		{ ExactBound.pow base exp }
	|	LPAR; o = dual_op; e = expression_bound; COMMA; f = expression_bound; RPAR
		{ o e f }
	|	LPAR; o = single_op; e = expression_bound; RPAR
		{ o e }

%inline list_op :
	|	LISTSUM
		{ ExactBound.list_sum }
	|	LISTPROD
		{ExactBound.list_prod }

%inline dual_op :
	|	SUM
		{ ExactBound.sum }
	|	PROD	
		{ ExactBound.prod }

%inline single_op :
	|	COS
		{ ExactBound.cos }
	|	SIN
		{ ExactBound.sin }

const :
	|	v = FLOAT
		{ OurNum.of_float_string v } ;
	|	v = FRACTION
		{ OurNum.of_string v } ;
	|	v = INT
		{ OurNum.of_int v } ; 

var :
	|   var = VAR
		{ Var.of_string var } ;
