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

# Pin the used opam packages
ENV OPAM_PKGS_VERSION=e258e1424a5f4054abf6508395c56c324808f789

RUN cd /home/opam/opam-repository; git pull; git checkout ${OPAM_PKGS_VERSION} .
RUN opam update
RUN opam upgrade
RUN opam switch create -y 4.09.1+musl+static+flambda
RUN eval $(opam env)

# Auxiliary libraries which are needed to build the opam packages
RUN sudo apk add m4 python2 gmp-dev perl mpfr-dev --no-cache

# If you have enough threads/memory available increase the job count
RUN opam install -j8 z3 ocamlfind menhir cmdliner ppx_deriving batteries ppx_deriving_cmdliner fpath omake apron ocamlgraph ounit

# Set environment variables to include libraries added through opam
ENV PATH=/home/opam/.opam/4.09.1+musl+static+flambda/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/opam/src/main
ENV LD_LIBRARY_PATH=/home/opam/.opam/4.09.1+musl+static+flambda/lib:/home/opam/.opam/4.09.1+musl+static+flambda/lib/stublibs

WORKDIR /home/opam/build
RUN sudo chown opam:nogroup /home/opam/build

COPY --chown=opam:nogroup src ./src
COPY --chown=opam:nogroup OMakeroot .
COPY --chown=opam:nogroup OMakefile .
# Needed for tests
COPY --chown=opam:nogroup examples ./examples

RUN eval $(opam env)

RUN omake clean
RUN RELEASE=1 omake --depend
RUN RELEASE=1 omake test

###################################################################
######################## EXECUTABLE IMAGE #########################
###################################################################

# Use alpine because it is super small
FROM alpine:3.12 as koat2_probabilistic
LABEL author="Fabian Meyer"

WORKDIR /home/koat2

# Add executables
COPY --from=koat2_build /home/opam/build/src/main/koat2 app/src/main/koat2

# Add Probabilistic Examples
COPY examples/ProbabilisticExamples/paper examples

# Add scripts
COPY docker_entry.sh app/src/main/docker_entry.sh

WORKDIR /home/koat2/examples
ENV PATH=/home/koat2/app/src/main:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

ENTRYPOINT [ "docker_entry.sh" ]
