FROM ocaml/opam2:alpine as koat2_build
LABEL author="Fabian Meyer"
LABEL author="Marcel Hark"

RUN cd /home/opam/opam-repository && git pull
RUN opam update
RUN opam upgrade
RUN opam switch create -y 4.09.1+musl+static+flambda
RUN eval `opam env`
RUN opam init
RUN opam update
RUN opam upgrade
RUN eval `opam env`
RUN sudo apk add m4 python2 gmp-dev perl mpfr-dev --no-cache
RUN opam install z3 batteries -j2
RUN opam install -j2 menhir cmdliner ppx_deriving ppx_deriving_cmdliner fpath apron ocamlgraph ounit omake
RUN eval `opam env`

ENV PATH=/home/opam/.opam/4.09.1+musl+static+flambda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/opam/src/main
ENV LD_LIBRARY_PATH=/home/opam/.opam/4.09.1+musl+static+flambda/lib:/home/opam/.opam/4.09.1+musl+static+flambda/lib/stublibs

WORKDIR /home/opam

COPY --chown=opam:opam src ./src
COPY --chown=opam:opam OMakeroot .
COPY --chown=opam:opam OMakefile .

# Run Build command
RUN RELEASE=1 omake --depend

FROM alpine:3.12

RUN apk update
RUN apk upgrade
RUN apk add --no-cache bash

RUN adduser -D koat2
WORKDIR /home/koat2

COPY --chown=koat2:koat2 --from=koat2_build /home/opam/src/main/koat2.opt bin/koat2
COPY --chown=koat2:koat2 examples/ ./examples

# Update PATH to include the added executables
ENV PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/koat2/bin

# Setup working environment, switch off of super user
USER koat2
# ENTRYPOINT ["koat2 analyse"]
ENTRYPOINT ["/bin/bash"]
