#!/usr/bin/env bash
cd examples/ProbabilisticExamples/paper
for i in *.koat; do
  echo \# $i
  # Adapt the timeout as needed
  timeout 300s ../../../src/main/koat2 analyse -i $i
done
