#############################################################
# Dockerfile to build KoAT2 Probabilistic
# Based on Ocaml 4.06.1
#############################################################

###################################################################
########################## BUILD IMAGE ############################
###################################################################

# Use the official opam image with opam 2
FROM ocaml/opam2:alpine as koat2_build
LABEL author="Fabian Meyer"

RUN sudo apk add m4

RUN opam switch create -y 4.07.1
RUN opam update
RUN opam upgrade
RUN eval $(opam env)

# Needed to compile Z3
RUN sudo apk add python2 --no-cache
RUN sudo apk add gmp-dev --no-cache

# Test
RUN sudo apk add libgcc libgomp libstdc++ musl

# Go and grab a cub of coffee, or two..
RUN opam install z3
RUN opam install ocamlfind menhir cmdliner ppx_deriving batteries ppx_deriving_cmdliner fpath omake

RUN sudo apk add perl --no-cache
RUN sudo apk add mpfr-dev --no-cache

RUN opam install apron ocamlgraph
RUN opam install ounit

# Set environment variables to include added libraries
ENV PATH=/home/opam/.opam/4.07.1/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/opam/src/main
ENV LD_LIBRARY_PATH=/home/opam/.opam/4.07.1/lib:/home/opam/.opam/4.07.1/lib/stublibs:/home/opam/.opam/4.07.1/lib/z3

WORKDIR /home/opam/build
COPY src ./src
COPY OMakeroot .
COPY OMakefile .
RUN sudo chown -R opam:nogroup .
