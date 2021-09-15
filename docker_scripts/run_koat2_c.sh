#!/usr/bin/env bash
clang -c ${@: -1} -emit-llvm -o out.bc # last argument i.e. file name
llvm2kittel -uniform-complexity-tuples -division-constraint exact out.bc > out
koat2 analyse -i ${@:2:$#-2} out
