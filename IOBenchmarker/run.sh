#! /bin/bash

SRC_PREFIX=parellel_io_test

if [ $# == 3 ]; then
    echo "Using buffer size : $3"
fi

#compile
g++ ${SRC_PREFIX}.c -o bin/st_io 
g++ ${SRC_PREFIX}.c -o bin/mt_io -DMULTI_THREAD -lpthread 
echo "Compilation finished."

echo "IO using 1 thread, $2 megabytes: "
rm bin/test_*
time bin/st_io $1 $2

echo ""
echo ""
echo "IO using $1 threads, $2 megabytes: "
rm bin/test_*
time bin/mt_io $1 $2


