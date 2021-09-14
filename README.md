# KoatReimplementation with Controlflow - Refinement
Reimplementation of KoAT with componentwise monotonic bounds

## Build

1. Use `opam init` and follow the instructions to set up Opam and install OCaml 4.06.1 if necessary
2. `opam install -j$(nproc) . --deps-only`
      * requires the _gmp_ and _mpfr_ libraries
3. Run `omake` to compile KoAT2
4. You may add `export PATH=$PATH:"path-to-Koat2-repository/Koat2/src/main/"` (with the substituted path) into your .bashrc file to use `koat2` directly.
5. For Controlflow - Refinement download https://github.com/jesusjda/pyRankFinder/releases/download/v1.3.2/irankfinder_1.3.2_linux_x86_64.zip and unzip it. Add `export PATH=$PATH:"path-to-irankfinder-folder/irankfinder/"` (with the substituted path) into your .bashrc file such that KoAT2 is able to find iRankFinder.


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
- iRankFinder: https://github.com/costa-group/iRankFinder


## Run

1. After compiling the testprogram "test" is run, it can be found in the subdirectory test.
2. Run ./koat2 in main to get the help page of koat2
  For further information run ./koat2 commandname --help
  The main command proceeding a full analysis is the "analyse" command.
