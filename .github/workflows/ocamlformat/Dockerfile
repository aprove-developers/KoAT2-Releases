FROM ocaml/opam:alpine@sha256:c8252c887e7f137e73c54f4e704b50583810a3e71015e99a39c21a6010db6cdb

RUN opam install dune ocamlformat.0.27.0

COPY --chown=opam:opam format.sh /format.sh
