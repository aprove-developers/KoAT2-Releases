FROM ocaml/opam:alpine@sha256:c8252c887e7f137e73c54f4e704b50583810a3e71015e99a39c21a6010db6cdb

ARG OCAML_VERSION=5.2.1

# Use our fork of opam-repository for static Z3
RUN opam repo add --set-default ourrepo https://github.com/aprove-developers/opam-repository.git && \
    opam switch create -y $OCAML_VERSION --packages=ocaml-variants.$OCAML_VERSION+options && \
    opam repo remove default

RUN opam install dune ocamlformat.0.27.0

COPY --chown=opam:opam format.sh /format.sh
