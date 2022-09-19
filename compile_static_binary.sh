#!/usr/bin/env bash
#To be executed from the top directory of the repository
#The static binary is compiled in a Dockerfile and then copied into the main file
./build_container.sh
docker create -ti --name koat2_dummy koat2 bash
docker cp koat2_dummy:/koat2/bin/koat2 ./koat2_static
docker rm -f koat2_dummy
