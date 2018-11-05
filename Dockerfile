#############################################################
# Dockerfile to build KoAT2
# Based on Ocaml 4.06.1
#############################################################
# Use the official ocaml with opam version of Docker
FROM ubuntu:18.04 as koat2_build
SHELL ["/bin/bash", "-c"]
#Disable Mailutils
ENV DEBIAN_FRONTEND="noninteractive"
MAINTAINER Marcel Hark

######################## BEGIN INSTALLATION #################
# Set up environment for ocaml and opam
RUN echo "root:Docker!" | chpasswd
RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install -y opam m4 libgmp10 libmpfr-dev
# Install needed packages for OPAM 
RUN apt-get install -y  libgmp-ocaml-dev
# Adding a new user and login as this user
RUN useradd -ms /bin/bash opam_user
USER opam_user

#Set the working directory to /home/opam_user
WORKDIR /home/opam_user


# Setting up opam
RUN opam init
RUN eval `opam config env`
RUN opam switch 4.06.1
RUN opam update; opam upgrade; 
RUN opam install ocamlfind ocamlgraph menhir ounit cmdliner ppx_deriving batteries ppx_deriving_cmdliner fpath
RUN opam install --no-checksums apron
RUN opam install z3
RUN opam install omake
RUN eval `opam config env`
RUN echo 'eval `opam config env`'>>.bashrc
RUN echo 'export LD_LIBRARY_PATH="$(ocamlfind printconf destdir)":$CAML_LD_LIBRARY_PATH:"$(ocamlfind printconf destdir)/z3"'>>.bashrc
ENV PATH=/home/opam_user/.opam/4.06.1/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/opam_user/src/main
ENV LD_LIBRARY_PATH=/home/opam_user/.opam/4.06.1/lib:/home/opam_user/.opam/4.06.1/lib/stublibs:/home/opam_user/.opam/4.06.1/lib/z3

#Copy all files especially Omakeroot into the container at /home/opam_user and make opam_user the user with all rights
COPY --chown=opam_user:opam_user src /home/opam_user/src
COPY --chown=opam_user:opam_user OMakeroot /home/opam_user
COPY --chown=opam_user:opam_user OMakefile /home/opam_user

#Build KoAT with omake
RUN /bin/bash --login -c "echo $PATH"
RUN echo $LD_LIBRARY_PATH
RUN /bin/bash -c "omake --depend"

#Run koatP when container launches

FROM ubuntu:18.04
# SHELL ["/bin/bash", "-c"]
#Disable Mailutils
ENV DEBIAN_FRONTEND="noninteractive"
MAINTAINER Marcel Hark

# Set up environment for sage and koat
RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install -y opam
RUN apt-get install -y m4 libgmp10 libmpfr-dev
# Install needed packages for OPAM 
RUN apt-get install -y  libgmp-ocaml-dev
RUN apt-get install -y sagemath

# Adding a new user and login as this user
RUN useradd -ms /bin/bash koat2_user
USER koat2_user

#Set the working directory to /home/koat_user
WORKDIR /home/koat2_user

#Copy all files especially Omakeroot into the container at /home/koat_user and make koat_user the user with all rights
COPY --from=koat2_build --chown=koat2_user:koat2_user /home/opam_user/.opam/4.06.1/lib /home/koat2_user/.opam/4.06.1/lib

COPY --from=koat2_build --chown=koat2_user:koat2_user /home/opam_user/.opam/4.06.1/share /home/koat2_user/.opam/4.06.1/share

COPY --from=koat2_build --chown=koat2_user:koat2_user /home/opam_user/src/exact_runtime /home/koat2_user/src/exact_runtime

COPY --from=koat2_build --chown=koat2_user:koat2_user /home/opam_user/src/main /home/koat2_user/src/main

ENV LD_LIBRARY_PATH=/home/koat2_user/.opam/4.06.1/lib:/home/koat2_user/.opam/4.06.1/lib/stublibs:/home/koat2_user/.opam/4.06.1/lib/z3:/home/koat2_user/.opam/4.06.1/share/apron/lib

ENV PATH=/home/koat2_user/.opam/4.06.1/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/koat2_user/src/main

ENTRYPOINT ["koat2", "exact"]

#docker tag koatp aprove/exact_runtime
#docker push aprove/exact_runtime
