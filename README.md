# KoatReimplementation
Reimplementation of KoAT with componentwise monotonic bounds

## Build

1. use `opam init` and follow the instructions to set up Opam and install OCaml 4.06.1 if necessary
  2. `opam install ocamlfind ocamlgraph`
      * requires the _gmp_ and _mpfr_ libraries
  3. `opam install z3`
      * this builds the OCaml bindings and the _libz3.so_ library (in `~/.opam/system/lib/Z3` if the Opam configuration files were installed in the default location)
  4. update the `LD_LIBRARY_PATH` such that _libz3.so_ can be found. To be on the safe side write "eval `opam config env`" and "
export LD_LIBRARY_PATH="$(ocamlfind printconf destdir)/stublibs:${LD_LIBRARY_PATH}"" into your .bashrc file. If this still does not work add "
export LD_LIBRARY_PATH="$(ocamlfind printconf destdir):${LD_LIBRARY_PATH}"" 
  5. `opam install menhir ounit apron cmdliner ppx_deriving ppx_deriving_cmdliner batteries`
  6. `opam install omake`
  7. `omake`
  
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


## Run

1. After compiling the testprogram "test" is run, it can be found in the subdirectory test.
2. Run ./koat2 in main to get the help page of koat2
  For further information run ./koat2 commandname --help
  The main command proceeding a full analysis is the "analyse" command.
