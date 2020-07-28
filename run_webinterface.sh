#!/bin/bash

INPUT=${@: -1}
TIMEOUT=300

POSITIONAL=()
while [[ $# -gt 0 ]]
do
key="$1"

case $key in
    -t|--timeout)
    TIMEOUT="$2"
    shift # past argument
    shift # past value
    ;;
    -r|--result)
    RESULT="$2"
    shift # past argument
    shift # past value
    ;;
    -i|--input)
    INPUT="$2"
    shift # past argument
    shift # past value
    ;;
    *)
    shift
    shift
    ;;
esac
done
set -- "${POSITIONAL[@]}" # restore positional parameters

echo "INPUT  = ${INPUT}"
echo "RESULT     = ${RESULT}"
echo "TIMEOUT    = ${TIMEOUT}"
if [ -z ${RESULT} ]; then timeout ${TIMEOUT} koat2 analyse -i ${INPUT}; else timeout ${TIMEOUT} koat2 analyse -i ${INPUT} -r ${RESULT}; fi


