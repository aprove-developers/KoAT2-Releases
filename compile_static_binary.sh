#!/usr/bin/env bash
#To be executed from the top directory of the repository
#The static binary is compiled in a Dockerfile and then copied into the main file
./build_container.sh
if  command -v podman &> /dev/null; then
  podman create -ti --name koat2_dummy koat2 bash
  podman cp koat2_dummy:/home/koat2/bin/koat2 ./src/main/koat2_static
  podman rm -f koat2_dummy
else
  docker create -ti --name koat2_dummy koat2 bash
  docker cp koat2_dummy:/home/koat2/bin/koat2 ./src/main/koat2_static
  docker rm -f koat2_dummy
fi
