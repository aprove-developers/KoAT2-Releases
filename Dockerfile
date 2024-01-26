#-------------------------------------------
# Build KoAT
#-------------------------------------------
FROM ocaml/opam:alpine@sha256:7bc1fcafcff5c152ef57e23314781e903e5a91ffa13e9e40cd3a478687d958b6 as koat2_build
LABEL author="Eleanore Meyer"
LABEL author="Marcel Hark"
LABEL author="Nils Lommen"

WORKDIR /home/opam

ARG OCAML_VERSION=5.1.0

# Add graphviz for tests
RUN sudo apk add m4 python3 gmp-dev perl mpfr-dev graphviz zip autoconf --no-cache

# PPL
RUN wget https://www.bugseng.com/external/ppl/download/ftp/releases/1.2/ppl-1.2.tar.xz && \
    tar xfv ppl-1.2.tar.xz && \
    cd ppl-1.2 && \
    ./configure && \
    make -j$(nproc) && \
    sudo make install && \
    make -j$(nproc) installcheck

# Use our fork of opam-repository for static Z3
RUN opam repo add --set-default ourrepo https://github.com/aprove-developers/opam-repository.git && \
    opam switch create -y $OCAML_VERSION-musl+static+flambda --packages=ocaml-variants.$OCAML_VERSION+options,ocaml-option-static,ocaml-option-musl,ocaml-option-flambda && \
    opam repo remove default

COPY --chown=opam:opam koat2.opam .
RUN opam update && \
    opam install -j $(nproc) . --deps-only

COPY --chown=opam:opam examples ./examples

ENV PATH=/home/opam/.opam/$OCAML_VERSION+musl+static+flambda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/opam/src/main
ENV LD_LIBRARY_PATH=/home/opam/.opam/$OCAML_VERSION+musl+static+flambda/lib:/home/opam/.opam/$OCAML_VERSION+musl+static+flambda/lib/stublibs


COPY --chown=opam:opam dune ./dune
COPY --chown=opam:opam dune-project ./dune-project

COPY --chown=opam:opam test ./test
COPY --chown=opam:opam bin ./bin
COPY --chown=opam:opam src ./src

# Run Build command and strip binaries
ARG KOAT2_VERSION_STRING=UNKNOWN
RUN eval $(opam env) && \
    KOAT2_GIT_VERSION=$KOAT2_VERSION_STRING dune build --release && \
    dune runtest --release && \
    strip _build/default/bin/main.exe -o koat2

#-------------------------------------------
# Get llvm2kittel + clang to analyse C programs
#-------------------------------------------

FROM ubuntu:14.04 as koat2_c_utils

RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y cmake gcc g++ libgmp-dev git llvm zlib1g-dev wget xz-utils


# Build llvm2kittel
RUN git clone https://github.com/s-falke/llvm2kittel/ && \
    cd llvm2kittel && \
    mkdir build && \
    cd build && \
    cmake .. && \
    make

# Get clang
RUN wget https://releases.llvm.org/3.4/clang+llvm-3.4-x86_64-linux-gnu-ubuntu-13.10.tar.xz && \
    tar xfv clang+llvm-3.4-x86_64-linux-gnu-ubuntu-13.10.tar.xz && \
    rm *.tar.xz && \
    mv clang+llvm-3.4-x86_64-linux-gnu-ubuntu-13.10/bin/clang .

# Build SMTPushdown
FROM ocaml/opam:ubuntu-20.04-ocaml-4.12 as koat2_smt2

RUN git clone https://github.com/aprove-developers/SMTPushdown.git && \
    opam install ocamlbuild ocamlfind sexplib && \
    eval $(opam env) && \
    cd SMTPushdown/ && \
    make -j$(nproc) && \
    cp _build/convert.native SMTPushdown

#-------------------------------------------
# Final Image
#-------------------------------------------
FROM ubuntu:20.04

RUN apt-get -y update && \
    apt-get install --no-install-recommends -y bash vim coreutils graphviz libgmp-dev libtinfo5 glibc-source git python3-sympy

RUN mkdir /koat2
WORKDIR /koat2


COPY examples/Complexity_ITS ./examples/Complexity_ITS
COPY examples/Complexity_C_Integer ./examples/Complexity_C_Integer

# COPY source code for sympy python script
COPY python/SizeBoundSolvable.py ./python/SizeBoundSolvable.py

COPY --from=koat2_c_utils /llvm2kittel/build/llvm2kittel bin/llvm2kittel
COPY --from=koat2_c_utils /clang bin/clang

COPY --from=koat2_smt2 /home/opam/SMTPushdown/SMTPushdown bin/SMTPushdown

COPY docker_scripts/wrapper_script.sh bin/wrapper_script.sh
COPY docker_scripts/run_koat2_c.sh bin/run_koat2_c.sh
COPY docker_scripts/run_koat2_smt2.sh bin/run_koat2_smt2.sh

ENV PATH=$PATH:/koat2/bin

COPY --from=koat2_build /home/opam/koat2 bin/koat2

# ENV LD_LIBRARY_PATH=/lib:/usr/local/lib:/usr/lib
ENTRYPOINT ["wrapper_script.sh"]
