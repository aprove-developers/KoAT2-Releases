#!/bin/bash
#To be executed from the top directory of the repository
#The static binary is compiled in a Dockerfile and then copied into the main file
docker build -t koat2 .
docker create -ti --name koat2_dummy koat2 bash
docker cp koat2_dummy:/home/koat2/bin/koat2 ./src/main/koat2_static
docker rm -f koat2_dummy
