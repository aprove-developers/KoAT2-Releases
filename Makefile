FindComp = ocamlfind ocamlc
ZThree = -package Z3 -linkpkg

Default : PolyTest

Tools.mli : Tools.ml
	ocamlc -i Tools.ml>Tools.mli
Tools.cmi : Tools.mli
	ocamlc -c Tools.mli
Tools.cmo : Tools.ml
	ocamlc -c Tools.ml
Poly.mli :  Poly.ml Tools.ml
	$(FindComp) -i $(ZThree) Poly.ml>Poly.mli
Poly.cmi : Poly.mli 
	$(FindComp) -c $(ZThree) Poly.mli
Poly.cmo : Poly.mli Poly.ml
	$(FindComp) -c $(ZThree) Poly.ml
PolyTest.cmo : PolyTest.ml Poly.cmo Poly.cmi
	$(FindComp) -c $(ZThree) PolyTest.ml

PolyTest : Tools.cmo Poly.cmo PolyTest.cmo
	$(FindComp) -o PolyTest.byte $(ZThree) Tools.cmo Poly.cmo PolyTest.cmo
clean :
	rm *.cmo *.cmi *.mli *.byte
