#!/bin/bash
# ./test_folder path strategy
for filename in $1*.koat; do 
    echo "path ${filename}"
    if LANG=C koat2 analyse -i "${filename}" --local=twn  | grep -q "Inf" ; then
        if LANG=C koat2 analyse -i "${filename}" --local=twn  --relax-loops | grep -q "Inf" ; then
            echo "valid Inf"
        else 
           echo "improvement found"
        fi
    elif LANG=C koat2 analyse -i "${filename}" --local=twn   | grep -q "O(" ; then
        if LANG=C koat2 analyse -i "${filename}" --local=twn  --relax-loops | grep -q "O(" ; then
            echo "valid bound"
        else 
            echo "Wrong solution"
        fi
    else 
        echo "error"
    fi
done