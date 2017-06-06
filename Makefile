FindComp = ocamlfind ocamlc
ZThree = -package Z3 -linkpkg

Default : LocationTest
  
Poly.mli :  Poly.ml
	$(FindComp) -i $(ZThree) Poly.ml>Poly.mli
Poly.cmi : Poly.mli Poly.ml
	$(FindComp) -c $(ZThree) Poly.mli
Poly.cmo : Poly.mli Poly.ml
	$(FindComp) -c $(ZThree) Poly.ml
Location.mli :  Location.ml
	ocamlc -i Location.ml>Location.mli
Location.cmi : Location.mli Location.ml
	ocamlc -c Location.mli
Location.cmo : Location.mli Location.ml
	ocamlc -c Location.ml
LocationTest.cmo : LocationTest.ml
	ocamlc -c LocationTest.ml
LocationTest : Location.cmi Location.cmo LocationTest.cmo 
	ocamlc  -o LocationTest.byte Location.cmo LocationTest.cmo
PolyTest.cmo : PolyTest.ml Poly.cmo Poly.cmi
	$(FindComp) -c $(ZThree) PolyTest.ml

PolyTest : Poly.ml Poly.cmi Poly.cmo PolyTest.cmo
	$(FindComp) -o PolyTest.byte $(ZThree) Poly.cmo PolyTest.cmo
clean :
	rm *.cmo *.cmi *.mli *.byte
