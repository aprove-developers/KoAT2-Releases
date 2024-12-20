#!/usr/bin/env bash
docker build -t koat2 $@ --build-arg KOAT2_VERSION_STRING=$(git describe --always --dirty --abbrev=7) .
