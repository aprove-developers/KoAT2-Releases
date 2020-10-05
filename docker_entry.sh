#!/usr/bin/env sh

if [ "$1" == "run-all" ]; then
  for i in *.koat; do
    echo \# $i
    time koat2 analyse -i $i --bottom-up
  done
else
  time koat2 analyse "$@"
fi
