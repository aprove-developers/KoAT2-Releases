(lang dune 3.3)

(using menhir 2.1)

(name koat2)

(version 2.0.0)

(generate_opam_files true)

(source
 (github aprove-developers/Koat2))

(authors "Eleanore Meyer")

(maintainers "eleanore.meyer@cs.rwth-aachen.de")

(license LICENSE)

(package
 (name koat2)
 (synopsis "KoAT2: Runtime Analysis of Integer Programs")
 (depends
   apron
   batteries
   cmdliner
   conf-gmp
   conf-mpfr
   conf-ppl
   (dune (< 3.8.0))
   fpath
   lens
   menhir
   metapp
   ocamlgraph
   ocamlnet
   odoc
   ounit
   parmap
   ppx_deriving
   ppx_deriving_cmdliner
   zarith
   z3
   )
 )


