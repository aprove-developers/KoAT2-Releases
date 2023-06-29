#!/usr/bin/env bash

./bin/SMTPushdown -convertto koat ${@: -1} > out
timeout 300s koat2 ${@:2:$#-2} out
