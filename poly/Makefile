FindComp = ocamlfind ocamlc
ZThree = -package Z3 -linkpkg

Default : PolynomialsTest

Tools.cmi : Tools.mli
	ocamlc -c Tools.mli
Tools.cmo : Tools.ml
	ocamlc -c Tools.ml

Mapping : Mapping.cmi Mapping.cmo
	
Mapping.cmi : Mapping.mli 
	$(FindComp) -c Mapping.mli
Mapping.cmo : Mapping.cmi Mapping.ml
	$(FindComp) -c Mapping.ml

Variables : Variables.cmi Variables.cmo

Variables.cmi : Variables.mli Mapping.cmo
	$(FindComp) -c $(ZThree) Mapping.cmo Variables.mli
Variables.cmo : Variables.cmi Variables.ml Mapping.cmo
	$(FindComp) -c $(ZThree) Mapping.cmo Variables.ml
	
Powers : Powers.cmi Powers.cmo

Powers.cmi : Powers.mli Variables.cmo Tools.cmo 
	$(FindComp) -c $(ZThree) Variables.cmo Tools.cmo Powers.mli
Powers.cmo : Powers.cmi Powers.ml Variables.cmo Tools.cmo
	$(FindComp) -c $(ZThree) Variables.cmo Tools.cmo Powers.ml
	
Monomials : Monomials.cmi Monomials.cmo

Monomials.cmi : Monomials.mli Powers.cmo Tools.cmo 
	$(FindComp) -c $(ZThree) Powers.cmo Tools.cmo Monomials.mli
Monomials.cmo : Monomials.cmi Monomials.ml Powers.cmo Tools.cmo
	$(FindComp) -c $(ZThree) Powers.cmo Tools.cmo Monomials.ml
	
ScaledMonomials : ScaledMonomials.cmi ScaledMonomials.cmo

ScaledMonomials.cmi : ScaledMonomials.mli Monomials.cmo Tools.cmo 
	$(FindComp) -c $(ZThree) Monomials.cmo Tools.cmo ScaledMonomials.mli
ScaledMonomials.cmo : ScaledMonomials.cmi ScaledMonomials.ml Monomials.cmo Tools.cmo
	$(FindComp) -c $(ZThree) Monomials.cmo Tools.cmo ScaledMonomials.ml

Polynomials : Polynomials.mli Polynomials.cmi Polynomials.cmo

Polynomials.mli : Polynomials.ml ScaledMonomials.cmo Tools.cmo
	$(FindComp) -i $(ZThree) ScaledMonomials.cmo Tools.cmo Polynomials.ml>Polynomials.mli
Polynomials.cmi : Polynomials.mli ScaledMonomials.cmo Tools.cmo 
	$(FindComp) -c $(ZThree) ScaledMonomials.cmo Tools.cmo Polynomials.mli
Polynomials.cmo : Polynomials.cmi Polynomials.ml ScaledMonomials.cmo Tools.cmo
	$(FindComp) -c $(ZThree) ScaledMonomials.cmo Tools.cmo Polynomials.ml
	
	
PolynomialsTest.cmo : PolynomialsTest.ml Polynomials.cmo Polynomials.cmi
	$(FindComp) -c $(ZThree) PolynomialsTest.ml

PolynomialsTest : Tools.cmo Mapping.cmo Variables.cmo Powers.cmo Monomials.cmo ScaledMonomials.cmo Polynomials.cmo PolynomialsTest.cmo
	$(FindComp) -o PolyTest.byte $(ZThree) Tools.cmo Mapping.cmo Variables.cmo Powers.cmo Monomials.cmo ScaledMonomials.cmo Polynomials.cmo PolynomialsTest.cmo
clean :
	rm -f *.cmo *.cmi *.byte
