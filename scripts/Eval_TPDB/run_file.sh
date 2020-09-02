#! /usr/bin/env zsh

KOAT=/home/fm/git/Hiwi/Koat/probabilistic/src/main/koat2

INP=$1

# HEADER
echo \#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
echo $(wc -l $INP)
echo \#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
echo

{time sh -c "timeout 300s $KOAT analyse -i $INP | head -1"} 2>&1

echo
echo
