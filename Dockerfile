#-------------------------------------------
# Build KoAT (the static binary of the Nix flake)
#-------------------------------------------
FROM nixos/nix:2.32.1@sha256:7767900b84b1f9d7d3108f86deee607a48e0edacd1075832af6a05864c4e8f40 AS koat2_build
LABEL author="Eleanore Meyer"
LABEL author="Marcel Hark"
LABEL author="Nils Lommen"

RUN echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

WORKDIR /koat2

# The dependencies only depend on these files. Building them first keeps them
# cached when only the sources change.
COPY flake.nix flake.lock koat2.opam koat2_external.opam ./
RUN nix develop .#staticDev --command true

COPY dune dune-project LICENSE README.md ./
COPY bin ./bin
COPY src ./src
COPY test ./test
COPY external ./external
# For the tests
COPY examples ./examples

# Build and test the static binary
ARG KOAT2_VERSION_STRING=UNKNOWN
# (The image has no sed, but the flake's nixpkgs does)
RUN nix shell --inputs-from . nixpkgs#gnused --command \
      sed -i "s/= \"VERSION_STR\"/= \"$KOAT2_VERSION_STRING\"/" src/util/VersionString.ml && \
    nix build .#koat2-static && \
    cp result/bin/koat2 koat2

#-------------------------------------------
# Get llvm2kittel + clang to analyse C programs
#-------------------------------------------

FROM ubuntu:14.04 AS koat2_c_utils

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
FROM ocaml/opam:ubuntu-20.04-ocaml-4.12 AS koat2_smt2

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
    apt-get install --no-install-recommends -y bash vim coreutils graphviz libgmp-dev libtinfo5 glibc-source git

RUN mkdir /koat2
WORKDIR /koat2


COPY examples/Complexity_ITS ./examples/Complexity_ITS
COPY examples/Complexity_C_Integer ./examples/Complexity_C_Integer

COPY --from=koat2_c_utils /llvm2kittel/build/llvm2kittel bin/llvm2kittel
COPY --from=koat2_c_utils /clang bin/clang

COPY --from=koat2_smt2 /home/opam/SMTPushdown/SMTPushdown bin/SMTPushdown

COPY docker_scripts/wrapper_script.sh bin/wrapper_script.sh
COPY docker_scripts/run_koat2_c.sh bin/run_koat2_c.sh
COPY docker_scripts/run_koat2_smt2.sh bin/run_koat2_smt2.sh

ENV PATH=$PATH:/koat2/bin

COPY --from=koat2_build /koat2/koat2 bin/koat2

# ENV LD_LIBRARY_PATH=/lib:/usr/local/lib:/usr/lib
ENTRYPOINT ["wrapper_script.sh"]
