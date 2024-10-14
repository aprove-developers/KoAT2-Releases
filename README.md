# KoAT2
Reimplementation of KoAT (see [here](https://koat.verify.rwth-aachen.de/) for more information) with control-flow refinement, multiphase ranking functions, runtime bounds by twn-loops, and size bounds by closed forms.

## Build

### Build directly from source code

1. Make sure [opam](https://opam.ocaml.org/) is installed
2. Use `opam init https://github.com/aprove-developers/opam-repository.git` and follow the instructions to set up Opam and install OCaml if necessary
3. `opam install -j$(nproc) . --deps-only`
4. Run `dune build` to build KoAT2

### Build [Docker](https://www.docker.com/) image

1. Install docker as described [here](https://docs.docker.com/engine/install/).
2. Execute `./build_container.sh` in the topdirectory of this repository. This builds a docker image with the name `koat2`. The docker image contains a static binary.

## Static Binary

To get a statically linked binary, please execute the script `compile_static_binary.sh`.
This will set up a docker container in which OCaml 4.09.1 for compiling static binaries using musl is installed with all required packages to compile KoAT2.
Please note that in this case, the used libraries (see below) are linked _statically_ into the resulting binary.
The code of these libraries can be found when following the given links below.

## Run

1. After compiling the binaries are located in `_build/install/bin`.
2. Run `./koat2` to get the help page of koat2.
  For further information run ./koat2 commandname --help.
  The main command proceeding a full analysis is the "analyse" command.
3. The project can be built and run by dune directly with the command `dune exec koat2 --`, or you may `export PATH=$PATH:"path-to-koat/_build/install/default/bin"` (with the substituted path) into your .bashrc file to use `koat2` directly.

## Formatting with `ocamlformat`
We use `ocamlformat` for formatting purposes.
Currently, we use version 0.26.1 which can be installed with `opam install ocamlformat.0.26.1`.
Use `dune build @fmt` to run the formatter, which will output the suggested changes.
You can accept these changes by executing `dune promote`.
Alternatively, you can format the code and directly accept all changes with `dune build @fmt --auto-promote`.

The directory `./hooks/`contains a pre-commit hook that automatically runs `dune build @fmt`.
To activate it, simply copy `./hooks/pre-commit` to `./.git/hooks/pre-commit`.

Note that the CI automatically runs `ocamlformat` upon push to autoformat the code.

## Tests & documentation

1. Documentation is build via the dune target `@doc`. Execute the command `dune build @doc`. The resulting documentation can be found in `_build/default/_doc/`.
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
- Algebraic Numbers by Isabelle/HOL: https://www.isa-afp.org/entries/Algebraic_Numbers.html

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
KoAT uses SymPy for some linear algebra calculations. If you want to use it, please install it on your system. SymPy is already included in the Docker image.

To generate an image of the graph of an integer program, KoAT2 invokes [Graphviz](https://graphviz.org/), which has to be manually installed on the system.
Note that when building the Docker image, the current Ubuntu binaries of [Graphviz](https://graphviz.org/) are installed.
Thus, the resulting image already contains [Graphviz](https://graphviz.org/).

Moreover, the Docker image also contains both [clang](https://clang.llvm.org/) and [llvm2kittel](https://github.com/s-falke/llvm2kittel) which are used to transform C programs into the input format of KoAT.

## Input Format

To analyze programs with KoAT2, they need to be represented as *Integer Transition Systems*.
We use an extended version of [KoAT's input format](https://koat.verify.rwth-aachen.de/getting_started/input_format/), which is also used in the category *Complexity of Integer Transition Systems* at the annual *Termination and Complexity Competition*.

In this extension, rules can be annotated with polynomial costs:

```
l1(A,B) -{A^2,A^2+B}> l2(A,B) :|: B >= 0
```
Here, `A^2` and `A^2+B` are lower and upper bounds on the cost of the rule.
The lower bound is ignored by KoAT2.
The upper bound has to be non-negative for every model of the transition's guard.

At the moment, we do not support recursion for complexity analysis, i.e., we only support `Com_1`, which can also be omitted.

## Contact
If you encounter any difficulties either use GitHub issues or contact aprove [at] i2.informatik.rwth-aachen.de
