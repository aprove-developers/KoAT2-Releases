# KoAT2
Reimplementation of KoAT (see [here](https://koat.verify.rwth-aachen.de/) for more information) with control-flow refinement, multiphase ranking functions, runtime bounds by twn-loops, and size bounds by closed forms.

## Build

### Build directly from source code

1. Use `opam init https://github.com/aprove-developers/opam-repository.git` and follow the instructions to set up Opam and install OCaml 4.06.1 if necessary
2. `opam install -j$(nproc) . --deps-only`
      * requires the _gmp_ and _mpfr_ libraries
3. Run `dune build` to build KoAT2

### Build [Docker](https://www.docker.com/) image

1. Install docker as described [here](https://docs.docker.com/engine/install/).
2. Execute `./build_container.sh` in the topdirectory of this repository. This builds a docker image with the name `koat2`. The docker image contains a static binary.

## Static Binary

To get a statically linked binary, please execute the script `compile_static_binary.sh`. This will set up a docker container in which OCaml 4.09.1 for compiling static binaries using musl is installed with all required packages to compile KoAT2. Please note that in this case, the used libraries (see below) are linked _statically_ into the resulting binary. The code of these libraries can be found when following the given links below.

## Run

1. After compiling the binaries are located in `_build/install/bin`.
2. Run `./koat2` to get the help page of koat2
  For further information run ./koat2 commandname --help
  The main command proceeding a full analysis is the "analyse" command.
3. The project can be built and run by dune directly with the command `dune exec koat2 --`, or you may `export PATH=$PATH:"path-to-koat/_build/install/default/bin"` (with the substituted path) into your .bashrc file to use `koat2` directly.
4. For Controlflow - Refinement download https://github.com/jesusjda/pyRankFinder/releases/download/v1.3.2/irankfinder_1.3.2_linux_x86_64.zip and unzip it. Add `export PATH=$PATH:"path-to-irankfinder-folder/irankfinder/"` (with the substituted path) into your .bashrc file such that KoAT2 is able to find iRankFinder.
5. For size bound technique based on closed forms, install [SymPy](https://www.sympy.org/en/index.html) on your system and use the flag `--closed-form-size-bounds`.`

## Formatting with `ocamlformat`
We use `ocamlformat` for formatting purposes.
Currently, we use version 0.25.1 which can be installed with `opam install ocamlformat.0.25.1`.
Use `dune build @fmt` to run the formatter, which will output the suggested changes.
You can accept these changes by executing `dune promote`.
Alternatively, you can format the code and directly accept all changes with `dune build @fmt --auto-promote`.

The directory `./hooks/`contains a pre-commit hook that automatically runs `dune build @fmt`.
To activate it, simply copy `./hooks/pre-commit` to `./.git/hooks/pre-commit`.

Note that the CI automatically runs `ocamlformat` upon push to autoformat the code.

## Tests & documentation

1. Documentation is build via the dune target `@doc`. Execute the command `dune build @doc`. The resultion documentation can be found in `_build/default/_doc/`
2. Running tests is done with the command `dune runtest`

## Links

- Ocaml Forum: https://discuss.ocaml.org/
- Opam: https://opam.ocaml.org/
- Dune: https://dune.readthedocs.io
- Batteries Included: http://ocaml-batteries-team.github.io/batteries-included/hdoc2/
- OUnit: http://ounit.forge.ocamlcore.org/api-ounit/index.html
- Menhir: http://gallium.inria.fr/~fpottier/menhir/
- Ocamlgraph: http://ocamlgraph.lri.fr/doc/
- Cmdliner: http://erratique.ch/software/cmdliner/doc/Cmdliner
- iRankFinder: https://github.com/costa-group/iRankFinder
- SymPy: https://www.sympy.org/en/index.html

## Used Libraries
KoAT2 makes use of the following external tools and libraries.

- [APRON](<https://antoinemine.github.io/Apron/doc/>)
- [Batteries](<http://ocaml-batteries-team.github.io/batteries-included/hdoc2/>)
- [Cmdliner](<http://erratique.ch/software/cmdliner/doc/Cmdliner>)
- [FPath](<https://erratique.ch/software/fpath>)
- [metapp](<https://github.com/thierry-martinez/metapp>)
- [Ocamlgraph](<http://ocamlgraph.lri.fr/doc/>)
- [Parmap](<https://github.com/rdicosmo/parmap>)
- [ppx_deriving](<https://github.com/ocaml-ppx/ppx_deriving>)
- [ppx_deriving_cmdliner](<https://github.com/hammerlab/ppx_deriving_cmdliner>)
- [SymPy](https://www.sympy.org/en/index.html)
- [Z3](https://github.com/Z3Prover/z3)

## External Tools
In its analysis, KoAT2 may use control-flow refinement via partial evaluation (see [here](https://aprove-developers.github.io/ComplexityMprfCfr/) for more details). To this end, it uses the external tool [iRankFinder](http://irankfinder.loopkiller.com:8081/). To use it, please install it on your system. However, [iRankFinder](http://irankfinder.loopkiller.com:8081/) is already included in the Docker image.

KoAT uses SymPy for some linear algebra calculations. If you want to use it, please install it on your system. SymPy is already included in the Docker image.

To generate an image of the graph of an integer program, KoAT2 invokes [Graphviz](https://graphviz.org/), which has to be manually installed on the system. Note that when building the Docker image, the current Ubuntu binaries of [Graphviz](https://graphviz.org/) are installed. Thus, the resulting image already contains [Graphviz](https://graphviz.org/).

Moreover, the Docker image also contains both [clang](https://clang.llvm.org/) and [llvm2kittel](https://github.com/s-falke/llvm2kittel) which are used to transform C programs into the input format of KoAT.

## Input Format

To analyze programs with KoAT2, they need to be represented as *Integer Transition Systems*.
We use an extended version of [KoAT's input format](http://aprove.informatik.rwth-aachen.de/eval/IntegerComplexity/), which is also used in the category *Complexity of Integer Transition Systems* at the annual *Termination and Complexity Competition*.

In this extension, rules can be annotated with polynomial costs:

```
l1(A,B) -{A^2,A^2+B}> l2(A,B) :|: B >= 0
```
Here, `A^2` and `A^2+B` are lower and upper bounds on the cost of the rule.
The lower bound is ignored by KoAT2.
The upper bound has to be non-negative for every model of the transition's guard.

At the moment, we do not support recursion, i.e., we only support `Com_1`, which can also be omitted.

## API
You can find our [API](<https://aprove-developers.github.io/KoAT2-Releases/index.html>) on GitHub Pages. We are still improving the documentation.

## Contact
If you encounter any difficulties either use GitHub issues or contact aprove [at] i2.informatik.rwth-aachen.de
