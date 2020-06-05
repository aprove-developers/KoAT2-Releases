#!/usr/bin/env bash
cd examples/ProbabilisticExamples/paper
for i in *.koat; do
  echo \# $i
  timeout 5s ../../../src/main/koat2 analyse -i $i --bottom-up
done
