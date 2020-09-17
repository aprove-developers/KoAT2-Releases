# KoAT's Reimplementation KoAT2 - Analysis of Expected Runtimes
Implementation of the techniques from our work [Inferring Expected Runtimes Using Expected Sizes](https://aprove-developers.github.io/ExpectedUpperBounds/)

## Build

### Build directly from source code

1. use `opam init` and follow the instructions to set up Opam and install OCaml 4.09.1 if necessary
2. `opam install ocamlfind ocamlgraph`
    * requires the _gmp_ and _mpfr_ libraries
3. `opam install z3`
    * this builds the OCaml bindings and the _libz3.so_ library (in `~/.opam/system/lib/Z3` if the Opam configuration files were installed in the default location)
4. update the `LD_LIBRARY_PATH` such that _libz3.so_ can be found. To be on the safe side write "eval `opam config env`" and "
export LD_LIBRARY_PATH="$(ocamlfind printconf destdir)/stublibs:${LD_LIBRARY_PATH}"" into your .bashrc file. If this still does not work add "
export LD_LIBRARY_PATH="$(ocamlfind printconf destdir):${LD_LIBRARY_PATH}"" 
5. `opam install menhir ounit apron cmdliner ppx_deriving ppx_deriving_cmdliner batteries ocamlnet`
6. `opam install omake`
7. `omake`

### Build [Docker](https://www.docker.com/) image

1. Install docker as described [here](https://docs.docker.com/engine/install/).
2. Execute `docker build -t koat -f Dockerfile .` in the topdirectory of this repository. This builds a docker image with the name `koat`. Details on how to execute this docker image can be found [here](https://aprove-developers.github.io/ExpectedUpperBounds/).
  
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
- Ocamlnet: http://projects.camlcity.org/projects/ocamlnet.html


## Run

1. After compiling the testprogram "test" is run, it can be found in the subdirectory test.
2. Run ./koat2 in main to get the help page of koat2
  For further information run ./koat2 commandname --help
  The main command proceeding a full analysis is the command "analyse".
3. A detailed description of the input format and how to execute this tool can be found [here](https://aprove-developers.github.io/ExpectedUpperBounds/). 
