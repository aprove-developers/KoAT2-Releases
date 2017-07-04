FindComp = ocamlfind ocamlopt -I ../poly
ZThree = -package Z3 -linkpkg
PolyDir = ../poly

.PHONY : Default clean PolynomialConstraintsAtom
Default : PolynomialConstraintsTest

Polynomials.cmx : 
	$(MAKE) Polynomials.cmx -C $(PolyDir)

PolynomialConstraintsAtoms : PolynomialConstraintsAtoms.mli PolynomialConstraintsAtoms.cmi PolynomialConstraintsAtoms.cmx PolynomialConstraintsTest.cmx PolynomialConstraintsTest
	
PolynomialConstraintsAtoms.mli : ../poly/Tools.cmx ../poly/Polynomials.cmx PolynomialConstraintsAtoms.ml
	$(FindComp) -i $(ZThree) ../poly/Tools.cmx ../poly/Polynomials.cmx PolynomialConstraintsAtoms.ml>PolynomialConstraintsAtoms.mli

PolynomialConstraintsAtoms.cmi : Polynomials.cmx PolynomialConstraintsAtoms.mli
	$(FindComp) -c $(ZThree) Tools.cmx Polynomials.cmx PolynomialConstraintsAtoms.mli

PolynomialConstraintsAtoms.cmx : PolynomialConstraintsAtoms.cmi PolynomialConstraintsAtoms.ml
	$(FindComp) -c $(ZThree) Tools.cmx Polynomials.cmx PolynomialConstraintsAtoms.ml
	
PolynomialConstraintsTest.cmx : PolynomialConstraintsTest.ml PolynomialConstraintsAtoms.cmx PolynomialConstraintsAtoms.cmi
	$(FindComp) -c $(ZThree) PolynomialConstraintsTest.ml

PolynomialConstraintsTest : PolynomialConstraintsTest.cmx Polynomials.cmx PolynomialConstraintsAtoms.cmx
	$(FindComp) -o PolynomialConstraintsTest.native $(ZThree) Tools.cmx Mapping.cmx Variables.cmx Powers.cmx Monomials.cmx ScaledMonomials.cmx Polynomials.cmx PolynomialConstraintsAtoms.cmx PolynomialConstraintsTest.cmx
	
clean :
	rm -f *.cmx *.cmi *.o *.native
