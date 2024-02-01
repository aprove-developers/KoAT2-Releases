#! /usr/bin/env bash

remove_substring() {
    local string="$1"
    local substring="$2"
    echo "${string//$substring/}"
}

if [[ "$*" == *"--webinterfaceITS"* ]]
then
    result=$(remove_substring "$*" "--webinterfaceITS")
    timeout 300s koat2 analyse $result
    KOAT_EXIT=$?
fi

if [[ "$*" == *"--webinterfaceProbITS"* ]]
then
    result=$(remove_substring "$*" "--webinterfaceProbITS")
    timeout 300s koat2 prob-analyse $result
    KOAT_EXIT=$?
fi

if [[ "$*" == *"--webinterfaceC"* ]]
then
    result=$(remove_substring "$*" "--webinterfaceC")
    timeout 300s run_koat2_c.sh c analyse $result
    KOAT_EXIT=$?
fi

if [[ "$*" == *"--webinterfaceSMT2"* ]]
then
    result=$(remove_substring "$*" "--webinterfaceSMT2")
    timeout 300s run_koat2_smt2.sh smt analyse $result
    KOAT_EXIT=$?
fi

# timeout returns 124
if [ $KOAT_EXIT -eq 124 ]; then
  echo TIMEOUT
  exit 0
fi

exit $KOAT_EXIT
