# KoatReimplementation
Reimplementation of cage Koat

## Build

1. use `opam init` and follow the instructions to set up Opam and install OCaml 4.04.2 if necessary
  2. `opam install ocamlfind ocamlgraph`
      * requires the _gmp_ and _mpfr_ libraries
  3. `opam remote add termite https://github.com/termite-analyser/opam-termite.git`
  4. `opam install Z3`
      * this builds the OCaml bindings and the _libz3.so_ library (in `~/.opam/system/lib/Z3` if the Opam configuration files were installed in the default location)
  5. update the `LD_LIBRARY_PATH` such that _libz3.so_ can be found _eg_ `export LD_LIBRARY_PATH=~/.opam/system/lib/Z3:$LD_LIBRARY_PA apron cmdliner ppx_deriving ppx_deriving_cmdliner batteries`
  6. `opam install menhir ounit`
  7. `opam install omake`
  8. `omake`
  
## Links

- Ocaml Forum: https://discuss.ocaml.org/
- Opam: https://opam.ocaml.org/
- Batteries Included: http://ocaml-batteries-team.github.io/batteries-included/hdoc2/
- Omake: http://omake.metaprl.org/manual/omake-doc.html
- Omake Recipes: http://www.camlcity.org/knowledge/kb_001_omake_recipes.html
- OUnit: http://ounit.forge.ocamlcore.org/api-ounit/index.html
- Menhir: http://gallium.inria.fr/~fpottier/menhir/
- Ocamlgraph: http://ocamlgraph.lri.fr/doc/
- Cmdliner: http://erratique.ch/software/cmdliner/doc/Cmdliner
