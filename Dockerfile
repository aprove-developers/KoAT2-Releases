#-------------------------------------------
# Build KoAT
#-------------------------------------------
FROM ocaml/opam:alpine as koat2_build
LABEL author="Eleanore Meyer"
LABEL author="Marcel Hark"

WORKDIR /home/opam

ARG OCAML_VERSION=4.11.2

# Use our fork of opam-repository for static Z3
RUN opam repo add --set-default ourrepo https://github.com/aprove-developers/opam-repository.git
RUN opam switch create -y $OCAML_VERSION+musl+static+flambda
RUN opam repo remove default
# Add graphviz for tests
RUN sudo apk add m4 python3 gmp-dev perl mpfr-dev graphviz zip --no-cache

# PPL
RUN wget https://www.bugseng.com/external/ppl/download/ftp/releases/1.2/ppl-1.2.tar.xz && \
    tar xfv ppl-1.2.tar.xz

RUN cd ppl-1.2 && \
    ./configure

RUN cd ppl-1.2 && \
    make -j$(nproc)

RUN cd ppl-1.2 && \
    sudo make install && \
    make -j$(nproc) installcheck

RUN wget -O "irankfinder.zip" https://github.com/jesusjda/pyRankFinder/releases/download/v1.3.1/irankfinder_v1.3.1_rhel7.zip && \
    mkdir irankfinder && \
    unzip irankfinder.zip -d irankfinder && \
    rm irankfinder.zip

COPY --chown=opam:opam opam .
RUN opam install -j $(nproc) . --deps-only

COPY --chown=opam:opam src ./src
COPY --chown=opam:opam OMakeroot .

COPY --chown=opam:opam OMakefile .
# needed to run tests
COPY --chown=opam:opam examples ./examples

ENV PATH=/home/opam/.opam/$OCAML_VERSION+musl+static+flambda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/opam/src/main
ENV LD_LIBRARY_PATH=/home/opam/.opam/$OCAML_VERSION+musl+static+flambda/lib:/home/opam/.opam/$OCAML_VERSION+musl+static+flambda/lib/stublibs

# Run Build command and strip binaries
ARG KOAT2_VERSION_STRING=UNKNOWN
RUN RELEASE=1 KOAT2_GIT_VERSION=$KOAT2_VERSION_STRING omake --depend && \
    strip src/main/koat2.opt

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


#-------------------------------------------
# Final Image
#-------------------------------------------
FROM ubuntu:20.04

RUN apt-get -y update && \
    apt-get install -y bash vim coreutils graphviz libgmp-dev libtinfo5

RUN mkdir /koat2
WORKDIR /koat2

COPY examples/Complexity_ITS ./examples/Complexity_ITS
COPY examples/Complexity_C_Integer ./examples/Complexity_C_Integer

COPY --from=koat2_build /home/opam/src/main/koat2.opt bin/koat2
COPY --from=koat2_build /home/opam/irankfinder ./irankfinder

COPY --from=koat2_c_utils /llvm2kittel/build/llvm2kittel bin/llvm2kittel
COPY --from=koat2_c_utils /clang bin/clang

COPY docker_scripts/wrapper_script.sh bin/wrapper_script.sh
COPY docker_scripts/run_koat2_c.sh bin/run_koat2_c.sh

RUN chmod +x /koat2/irankfinder/1.3.1/irankfinder/CFRefinement
#Update PATH to include the added executables
# ENV PATH=/koat2/irankfinder/1.3.1/irankfinder/partialevaluation/bin:/koat2/irankfinder/1.3.1/irankfinder/ppl:/koat2/irankfinder/1.3.1/irankfinder:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/koat2/bin
# ENV LD_LIBRARY_PATH=/koat2/irankfinder/1.3.1/irankfinder/ppl:/koat2/irankfinder/1.3.1/irankfinder/partialevaluation/bin:/koat2/irankfinder/1.3.1/irankfinder:/lib:/usr/local/lib:/usr/lib

ENV PATH=$PATH:/koat2/bin:/koat2/irankfinder/1.3.1/irankfinder/partialevaluation/bin:/koat2/irankfinder/1.3.1/irankfinder/ppl:/koat2/irankfinder/1.3.1/irankfinder
# ENV LD_LIBRARY_PATH=/lib:/usr/local/lib:/usr/lib
ENTRYPOINT ["wrapper_script.sh"]
