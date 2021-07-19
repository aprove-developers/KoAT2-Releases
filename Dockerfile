FROM ocaml/opam:alpine as koat2_build
LABEL author="Fabian Meyer"
LABEL author="Marcel Hark"

ARG OCAML_VERSION=4.11.2

RUN opam update
RUN opam upgrade
RUN opam switch create -y $OCAML_VERSION+musl+static+flambda
RUN eval `opam env`
RUN opam init
RUN opam update
RUN opam upgrade
RUN eval `opam env`
# Add graphviz for tests
RUN sudo apk add m4 python2 gmp-dev perl mpfr-dev graphviz --no-cache

WORKDIR /home/opam

COPY --chown=opam:opam opam .
RUN opam install -j $((`nproc` - 2)) . --deps-only

COPY --chown=opam:opam src ./src
COPY --chown=opam:opam OMakeroot .

COPY --chown=opam:opam OMakefile .
# needed to run tests
COPY --chown=opam:opam examples ./examples

RUN eval `opam env`

ENV PATH=/home/opam/.opam/$OCAML_VERSION+musl+static+flambda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/opam/src/main
ENV LD_LIBRARY_PATH=/home/opam/.opam/$OCAML_VERSION+musl+static+flambda/lib:/home/opam/.opam/$OCAML_VERSION+musl+static+flambda/lib/stublibs

# Run Build command
ARG KOAT2_VERSION_STRING=UNKNOWN
RUN RELEASE=1 KOAT2_GIT_VERSION=$KOAT2_VERSION_STRING omake --depend

FROM ubuntu:20.04

RUN apt-get -y update
RUN apt-get -y upgrade
RUN apt-get install -y bash vim coreutils
RUN apt-get install -y wget
RUN apt-get install -y zip

RUN adduser koat2
WORKDIR /home/koat2

COPY --chown=koat2:koat2 --from=koat2_build /home/opam/src/main/koat2.opt bin/koat2
COPY --chown=koat2:koat2 examples/Complexity_ITS ./examples

# Setup working environment, switch off of super user
USER koat2

RUN wget -O "irankfinder.zip" https://github.com/jesusjda/pyRankFinder/releases/download/v1.3.1/irankfinder_v1.3.1_rhel7.zip
RUN mkdir irankfinder
RUN unzip irankfinder.zip -d irankfinder
RUN rm irankfinder.zip
RUN chmod +x /home/koat2/irankfinder/1.3.1/irankfinder/CFRefinement
#Update PATH to include the added executables
ENV PATH=/home/koat2/irankfinder/1.3.1/irankfinder/partialevaluation/bin:/home/koat2/irankfinder/1.3.1/irankfinder/ppl:/home/koat2/irankfinder/1.3.1/irankfinder:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/koat2/bin
ENV LD_LIBRARY_PATH=/home/koat2/irankfinder/1.3.1/irankfinder/ppl:/home/koat2/irankfinder/1.3.1/irankfinder/partialevaluation/bin:/home/koat2/irankfinder/1.3.1/irankfinder:/usr/local/lib:/usr/lib
ENTRYPOINT ["koat2"]
