for filename in examples/**/*.koat; do 
    echo "path ${filename}"
    if LANG=C koat2 analyse -i "${filename}" --local=$1 --depth=5 | grep -q "Inf" ; then
        if LANG=C koat2 analyse -i "${filename}" --local=$1 --depth=5 --termination | grep -q "no" ; then
            echo "valid no"
        else 
           echo "improvement found"
        fi
    elif LANG=C koat2 analyse -i "${filename}" --local=$1 --depth=5 | grep -q "O(" ; then
        if LANG=C koat2 analyse -i "${filename}" --local=$1 --depth=5 --termination | grep -q "yes" ; then
            echo "valid yes"
        else 
            echo "Wrong solution"
        fi
    else 
        echo "error"
    fi
done
