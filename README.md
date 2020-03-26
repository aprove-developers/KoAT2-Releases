# KoAT2
Reimplementation of [KoAT](<https://github.com/s-falke/kittel-koat/>) with componentwise monotonic bounds

## Build
1. Use `opam init` and follow the instructions to set up Opam and install OCaml 4.07.1 if necessary

2. Execute `opam install ocamlfind ocamlgraph`
      * requires the _gmp_ and _mpfr_ libraries
      
3. Execute `opam install z3`
      * this builds the OCaml bindings and the _libz3.so_ library (in `~/.opam/system/lib/Z3` if the Opam configuration files were installed in the default location)
      
4. Update the `LD_LIBRARY_PATH` such that _libz3.so_ can be found. To be on the safe side write "eval `opam config env`" and "
export LD_LIBRARY_PATH="$(ocamlfind printconf destdir)/stublibs:${LD_LIBRARY_PATH}"" into your .bashrc file. If this still does not work add "
export LD_LIBRARY_PATH="$(ocamlfind printconf destdir):${LD_LIBRARY_PATH}"" 

5. Execute `opam install menhir ounit apron cmdliner ppx_deriving ppx_deriving_cmdliner batteries fpath omake`
  
6. Use the command `omake` to build koat2.
  
## Links

- Ocaml Forum: https://discuss.ocaml.org/
- Opam: https://opam.ocaml.org/
- Omake: http://omake.metaprl.org/manual/omake-doc.html
- Omake Recipes: http://www.camlcity.org/knowledge/kb_001_omake_recipes.html
- OUnit: http://ounit.forge.ocamlcore.org/api-ounit/index.html
- Menhir: http://gallium.inria.fr/~fpottier/menhir/

## Used Libraries
KoAT2 makes use of the following external tools and libraries.

- [APRON](<https://antoinemine.github.io/Apron/doc/>)
- [Batteries](<http://ocaml-batteries-team.github.io/batteries-included/hdoc2/>)
- [Cmdliner](<http://erratique.ch/software/cmdliner/doc/Cmdliner>)
- [FPath](<https://erratique.ch/software/fpath>)
- [Ocamlgraph](<http://ocamlgraph.lri.fr/doc/>)
- [ppx_deriving](<https://github.com/ocaml-ppx/ppx_deriving>)
- [ppx_deriving_cmdliner](<https://github.com/hammerlab/ppx_deriving_cmdliner>)
- [Z3](https://github.com/Z3Prover/z3)

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

## Run

1. After compiling the testprogram "test" is run, it can be found in the subdirectory test.
  *One* of the test *fails*, due to a wrong input format, which is a correct test for the given file.
2. Run ./koat2 in main to get the help page of koat2
  For further information run ./koat2 commandname --help
  The main command proceeding a full analysis is the "analyse" command.

## API
You can fand our [API](<https://aprove-developers.github.io/KoAT2-Releases/index.html>) on GitHub Pages. We are still improving the documentation.
