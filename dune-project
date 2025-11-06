(lang dune 3.3)

(using menhir 2.1)

(name koat2)

(version 2.0.0)

(generate_opam_files true)

(source
 (github aprove-developers/Koat2))

(authors "Eleanore Meyer" "Nils Lommen")

(maintainers "eleanore.meyer@cs.rwth-aachen.de" "lommen@cs.rwth-aachen.de")

(license LICENSE)

(package
 (name koat2_external)
 (synopsis "External (automatically generated files) for KoAT2")
 (depends (z3 (= 4.15.2)))
)

(package
 (name koat2)
 (synopsis "KoAT2: Runtime Analysis of Integer Programs")
 (depends
   apron
   (base (= v0.17.3))
   stdio
   batteries
   cmdliner
   conf-gmp
   conf-mpfr
   conf-ppl
   dune
   dune-build-info
   fpath
   lens
   menhir
   ocamlgraph
   (ocamlformat (= 0.27.0))
   odoc
   ounit
   parmap
   ppx_deriving
   ppx_deriving_cmdliner
   ppx_sexp_conv
   zarith
   (z3 (= 4.15.2))
   koat2_external
   ocolor
   )
 )
