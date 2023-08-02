#!/usr/bin/env bash
sed 's/typedef enum {false, true} bool;/const int false = 0;\nconst int true = 1;/g' ${@: -1} > replaced.c
clang -c replaced.c -emit-llvm -Wno-logical-op-parentheses -o out.bc # last argument i.e. file name
llvm2kittel -uniform-complexity-tuples -division-constraint exact out.bc > out
koat2 ${@:2:$#-2} out
