FindComp = ocamlfind ocamlopt
ZThree = -package Z3 -linkpkg

Default : PolynomialsTest

Tools.cmi : Tools.mli
	$(FindComp) -c Tools.mli
Tools.cmx : Tools.ml
	$(FindComp) -c Tools.ml

Mapping : Mapping.cmi Mapping.cmx
	
Mapping.cmi : Mapping.mli 
	$(FindComp) -c Mapping.mli
Mapping.cmx : Mapping.cmi Mapping.ml
	$(FindComp) -c Mapping.ml

Variables : Variables.cmi Variables.cmx

Variables.cmi : Variables.mli Mapping.cmx
	$(FindComp) -c $(ZThree) Mapping.cmx Variables.mli
Variables.cmx : Variables.cmi Variables.ml Mapping.cmx
	$(FindComp) -c $(ZThree) Mapping.cmx Variables.ml
	
Powers : Powers.cmi Powers.cmx

Powers.cmi : Powers.mli Variables.cmx Tools.cmx 
	$(FindComp) -c $(ZThree) Variables.cmx Tools.cmx Powers.mli
Powers.cmx : Powers.cmi Powers.ml Variables.cmx Tools.cmx
	$(FindComp) -c $(ZThree) Variables.cmx Tools.cmx Powers.ml
	
Monomials : Monomials.cmi Monomials.cmx

Monomials.cmi : Monomials.mli Powers.cmx Tools.cmx 
	$(FindComp) -c $(ZThree) Powers.cmx Tools.cmx Monomials.mli
Monomials.cmx : Monomials.cmi Monomials.ml Powers.cmx Tools.cmx
	$(FindComp) -c $(ZThree) Powers.cmx Tools.cmx Monomials.ml
	
ScaledMonomials : ScaledMonomials.cmi ScaledMonomials.cmx

ScaledMonomials.cmi : ScaledMonomials.mli Monomials.cmx Tools.cmx 
	$(FindComp) -c $(ZThree) Monomials.cmx Tools.cmx ScaledMonomials.mli
ScaledMonomials.cmx : ScaledMonomials.cmi ScaledMonomials.ml Monomials.cmx Tools.cmx
	$(FindComp) -c $(ZThree) Monomials.cmx Tools.cmx ScaledMonomials.ml

Polynomials : Polynomials.cmi Polynomials.cmx

#Polynomials.mli : Polynomials.ml ScaledMonomials.cmx Tools.cmx
#	$(FindComp) -i $(ZThree) ScaledMonomials.cmx Tools.cmx Polynomials.ml>Polynomials.mli

Polynomials.cmi : Polynomials.mli ScaledMonomials.cmx Tools.cmx 
	$(FindComp) -c $(ZThree) ScaledMonomials.cmx Tools.cmx Polynomials.mli
	
Polynomials.cmx : Polynomials.cmi Polynomials.ml ScaledMonomials.cmx Tools.cmx
	$(FindComp) -c $(ZThree) ScaledMonomials.cmx Tools.cmx Polynomials.ml
	
PolynomialsTest.cmx : PolynomialsTest.ml Polynomials.cmx Polynomials.cmi
	$(FindComp) -c $(ZThree) PolynomialsTest.ml

PolynomialsTest : Tools.cmx Mapping.cmx Variables.cmx Powers.cmx Monomials.cmx ScaledMonomials.cmx Polynomials.cmx PolynomialsTest.cmx
	$(FindComp) -o PolynomialsTest.native $(ZThree) Tools.cmx Mapping.cmx Variables.cmx Powers.cmx Monomials.cmx ScaledMonomials.cmx Polynomials.cmx PolynomialsTest.cmx
	
PolynomialConstraintsAtoms : PolynomialConstraintsAtoms.mli PolynomialConstraintsAtoms.cmi PolynomialConstraintsAtoms.cmx PolynomialConstraintsTest.cmx PolynomialConstraintsTest
	
PolynomialConstraintsAtoms.mli : Tools.cmx Polynomials.cmx PolynomialConstraintsAtoms.ml
	$(FindComp) -i $(ZThree) Tools.cmx Polynomials.cmx PolynomialConstraintsAtoms.ml>PolynomialConstraintsAtoms.mli

PolynomialConstraintsAtoms.cmi : Tools.cmx Polynomials.cmx PolynomialConstraintsAtoms.mli
	$(FindComp) -c $(ZThree) Tools.cmx Polynomials.cmx PolynomialConstraintsAtoms.mli

PolynomialConstraintsAtoms.cmx : PolynomialConstraintsAtoms.cmi PolynomialConstraintsAtoms.ml
	$(FindComp) -c $(ZThree) Tools.cmx Polynomials.cmx PolynomialConstraintsAtoms.ml
	
PolynomialConstraintsTest.cmx : PolynomialConstraintsTest.ml PolynomialConstraintsAtoms.cmx PolynomialConstraintsAtoms.cmi
	$(FindComp) -c $(ZThree) PolynomialConstraintsTest.ml

PolynomialConstraintsTest : PolynomialConstraintsTest.cmx Tools.cmx Mapping.cmx Variables.cmx Powers.cmx Monomials.cmx ScaledMonomials.cmx Polynomials.cmx PolynomialConstraintsAtoms.cmx
	$(FindComp) -o PolynomialConstraintsTest.native $(ZThree) Tools.cmx Mapping.cmx Variables.cmx Powers.cmx Monomials.cmx ScaledMonomials.cmx Polynomials.cmx PolynomialConstraintsAtoms.cmx PolynomialConstraintsTest.cmx
	
clean :
	rm -f *.cmx *.cmi *.byte *.native
