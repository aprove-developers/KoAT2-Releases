#!/usr/bin/env bash
docker build -t koat2 --build-arg KOAT2_VERSION_STRING="$(git rev-parse --short HEAD) from $(git log -1 --format=%cs)" .

