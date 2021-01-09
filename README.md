# KoatReimplementation with Controlflow - Refinement
Reimplementation of KoAT with componentwise monotonic bounds

## Build

1. Use `opam init` and follow the instructions to set up Opam and install OCaml 4.06.1 if necessary
2. `opam install ocamlfind ocamlgraph`
      * requires the _gmp_ and _mpfr_ libraries
3. `opam install z3`
      * this builds the OCaml bindings and the _libz3.so_ library (in `~/.opam/system/lib/Z3` if the Opam configuration files were installed in the default location)
4. Update the `LD_LIBRARY_PATH` such that _libz3.so_ can be found.
      * To be on the safe side write eval \`opam config env\` (If your are viewing the raw README.md file, remove both backslashes) and `export LD_LIBRARY_PATH="$(ocamlfind printconf destdir)/stublibs:${LD_LIBRARY_PATH}"` into your .bashrc file. 
      * If this still does not work add `export LD_LIBRARY_PATH="$(ocamlfind printconf destdir):${LD_LIBRARY_PATH}"` 
5. `opam install menhir ounit apron cmdliner ppx_deriving ppx_deriving_cmdliner batteries fpath omake`
6. `omake`
7. You may add `export PATH=$PATH:"path-to-Koat2-repository/Koat2/src/main/"` (with the substituted path) into your .bashrc file to use `koat2` directly.
8. For Controlflow - Refinement download https://github.com/jesusjda/pyRankFinder/releases/download/v1.3.2/irankfinder_1.3.2_linux_x86_64.zip and unzip it. Add `export PATH=$PATH:"path-to-irankfinder-folder/irankfinder/"` (with the substituted path) into your .bashrc file.

  
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
