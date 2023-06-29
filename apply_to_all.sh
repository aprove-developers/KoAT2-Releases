#!/bin/bash
# ./test_folder path strategy
for filename in $1*.koat; do 
    echo "path ${filename}"
    LANG=C koat2 analyse -i "${filename}" $2
    LANG=C koat2 analyse -i "${filename}" $3
done