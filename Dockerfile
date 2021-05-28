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
RUN opam install -j $((`nproc` - 2)) z3 batteries 
RUN opam install -j $((`nproc` - 2)) menhir cmdliner ppx_deriving ppx_deriving_cmdliner fpath apron ocamlgraph ounit omake ocamlnet
RUN eval `opam env`

ENV PATH=/home/opam/.opam/4.09.1+musl+static+flambda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/opam/src/main
ENV LD_LIBRARY_PATH=/home/opam/.opam/4.09.1+musl+static+flambda/lib:/home/opam/.opam/4.09.1+musl+static+flambda/lib/stublibs

WORKDIR /home/opam

COPY --chown=opam:opam src ./src
COPY --chown=opam:opam OMakeroot .
COPY --chown=opam:opam OMakefile .
COPY --chown=opam:opam examples ./examples

# Run Build command
RUN RELEASE=1 omake --depend

FROM ubuntu:20.04

RUN apt -y update
RUN apt -y upgrade
RUN apt install -y bash
RUN apt install -y wget
RUN apt install -y zip

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
