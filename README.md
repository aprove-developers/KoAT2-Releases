# KoAT's Reimplementation KoAT2 - Analysis of Expected Runtimes
Implementation of the techniques from our works [Computing Expected Runtimes for Constant Probability Programs](https://aprove-developers.github.io/recurrence/ and  [Inferring Expected Runtimes Using Expected Sizes](https://aprove-developers.github.io/ExpectedUpperBounds/)
Please note that a working installation of [Graphviz](https://www.graphviz.org/) is required to render the input programs as graphs.
Moreover, for using the technique for inferring [exact expected runtimes](https://aprove-developers.github.io/recurrence/) you need to install [Python3](https://www.python.org/) and its its library [SymPy](https://www.sympy.org/en/index.html) via the package manager [Pip](https://pypi.org/project/pip/) (even when using the static binary). 

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
2. Execute `docker build -t koat -f Dockerfile .` in the topdirectory of this repository. This builds a docker image with the name `koat`. The docker image contains a static binary. Details on how to execute this docker image can be found [here](https://aprove-developers.github.io/ExpectedUpperBounds/).

## Links

- Ocaml Forum: https://discuss.ocaml.org/
- Opam: https://opam.ocaml.org/
- Omake: http://omake.metaprl.org/manual/omake-doc.html
- Omake Recipes: http://www.camlcity.org/knowledge/kb_001_omake_recipes.html
- OUnit: http://ounit.forge.ocamlcore.org/api-ounit/index.html
- Menhir: http://gallium.inria.fr/~fpottier/menhir/
- Musl: https://www.musl-libc.org/
- Python: https://www.python.org/
- Pip: https://pypi.org/project/pip/
- SymPy: https://www.sympy.org/en/index.html

## Used Libraries
KoAT2 makes use of the following external libraries. In the static binary built inside the docker image, these libraries are linked into the binary.

- [APRON](<https://antoinemine.github.io/Apron/doc/>)
- [Batteries](<http://ocaml-batteries-team.github.io/batteries-included/hdoc2/>)
- [Cmdliner](<http://erratique.ch/software/cmdliner/doc/Cmdliner>)
- [FPath](<https://erratique.ch/software/fpath>)
- [Ocamlgraph](<http://ocamlgraph.lri.fr/doc/>)
- [Ocamlnet](http://projects.camlcity.org/projects/ocamlnet.html)
- [ppx_deriving](<https://github.com/ocaml-ppx/ppx_deriving>)
- [ppx_deriving_cmdliner](<https://github.com/hammerlab/ppx_deriving_cmdliner>)
- [Z3](https://github.com/Z3Prover/z3)

## External Tools
To generate an image of the graph of a probabilistic integer program, KoAT2 invokes [Graphviz](https://graphviz.org/), which has to be manually installed on the system. Note that when building the Docker image, the current alpine binaries of [Graphviz](https://graphviz.org/) are installed. Thus, the resulting image already contains [Graphviz](https://graphviz.org/). 

For using the technique for inferring [exact expected runtimes](https://aprove-developers.github.io/recurrence/) you need to install [Python3](https://www.python.org/) and its its library [SymPy](https://www.sympy.org/en/index.html) via the package manager [Pip](https://pypi.org/project/pip/) (even when using the static binary). 

## Run

1. After compiling, the testprogram `test` is run. It can be found in the subdirectory `test`.
2. Run `./koat2` in main to get the help page of `koat2`. For further information run `./koat2 commandname --help`. The main command proceeding a full analysis is the command `analyse`.
3. A detailed description of the different input formats and how to execute this tool can be found [here](https://aprove-developers.github.io/recurrence/) and [here](https://aprove-developers.github.io/ExpectedUpperBounds/).
