#############################################################
# Dockerfile to build KoAT2 Probabilistic
# Based on Ocaml 4.07.1
#############################################################

###################################################################
########################## BUILD IMAGE ############################
###################################################################

# Use the official opam image with opam 2
FROM ocaml/opam2:alpine as koat2_build
LABEL author="Fabian Meyer"

RUN opam switch create -y 4.07.1+flambda
RUN opam update
RUN opam upgrade
RUN eval $(opam env)

# Auxiliary libraries which are needed to build the opam packages
RUN sudo apk add m4 python2 gmp-dev perl mpfr-dev --no-cache

RUN opam install z3 ocamlfind menhir cmdliner ppx_deriving batteries ppx_deriving_cmdliner fpath omake apron ocamlgraph ounit

RUN eval $(opam env)

# Set environment variables to include libraries added through opam
ENV PATH=/home/opam/.opam/4.07.1+flambda/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/opam/src/main
ENV LD_LIBRARY_PATH=/home/opam/.opam/4.07.1+flambda/lib:/home/opam/.opam/4.07.1+flambda/lib/stublibs

WORKDIR /home/opam/build
RUN sudo chown opam:nogroup /home/opam/build

COPY --chown=opam:nogroup src ./src
COPY --chown=opam:nogroup OMakeroot .
COPY --chown=opam:nogroup OMakefile .
# Needed for tests
COPY --chown=opam:nogroup examples ./examples

RUN omake clean
RUN RELEASE=1 omake --depend
RUN cd src/test && ./Test

###################################################################
######################## EXECUTABLE IMAGE #########################
###################################################################

# Use alpine because it is super small
FROM alpine:3.9 as koat2_probabilistic
LABEL author="Fabian Meyer"

RUN adduser -D koat2
WORKDIR /home/koat2

# Install necessary packages
RUN apk add libstdc++ mpfr3 libgomp --no-cache

# Add executables
COPY --from=koat2_build --chown=koat2:koat2 /home/opam/build/src/main/koat2 app/src/main/koat2

# Add Probabilistic Examples
COPY --chown=koat2:koat2 examples/ProbabilisticExamples examples

USER koat2
WORKDIR /home/koat2/examples
ENV PATH=/home/koat2/app/src/main:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
ENTRYPOINT ["koat2", "analyse", "-i"]
