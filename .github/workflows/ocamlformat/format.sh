#!/usr/bin/env sh

cd "$1"
sudo -E env "PATH=$PATH" dune build @fmt --auto-promote
sudo -E env "PATH=$PATH" dune build @fmt --auto-promote
