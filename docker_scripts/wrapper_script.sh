#! /usr/bin/env bash

if [[ "$1" = "its" ]] ; then
  timeout 300s koat2 ${@:2}
  KOAT_EXIT=$?
fi
if [[ "$1" = "c" ]] ; then
  timeout 300s run_koat2_c.sh $@
  KOAT_EXIT=$?
fi
if [[ "$1" = "smt2" ]] ; then
  timeout 300s run_koat2_smt2.sh $@
  KOAT_EXIT=$?
fi
# timeout returns 124
if [[ $KOAT_EXIT -eq 124 ]] ; then
  echo TIMEOUT
  exit 0
fi

exit $KOAT_EXIT
