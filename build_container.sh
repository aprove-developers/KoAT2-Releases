#!/usr/bin/env bash
if  command -v podman &> /dev/null; then
  podman build -t koat2 --build-arg KOAT2_VERSION_STRING="$(git rev-parse --short HEAD) from $(git log -1 --format=%cs)" .
else
  docker build -t koat2 --build-arg KOAT2_VERSION_STRING="$(git rev-parse --short HEAD) from $(git log -1 --format=%cs)" .
fi;
