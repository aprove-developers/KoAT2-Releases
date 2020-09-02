#! /usr/bin/env zsh

DIR=/home/fm/git/Hiwi/TPDB

CONC_JOBS=8

ls $DIR/**/*.koat | parallel -j$CONC_JOBS --progress ./run_file.sh {}

