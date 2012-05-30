#! /bin/bash
OBJ=fsync_test

gcc fsync_test.c -o $OBJ

time ./$OBJ fsync

time ./$OBJ fdatasync

time ./$OBJ fdatasync_extend

rm $OBJ
