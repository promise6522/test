#! /bin/bash
g++ benchmark.c -ltcmalloc_minimal -o tcmalloc
g++ benchmark.c -lpthread -o ptmalloc

echo "Using tcmalloc : "
./tcmalloc $1 $2 $3 $4
#rm tcmalloc
echo ""

echo "Using ptmalloc2 : "
./ptmalloc $1 $2 $3 $4
#rm ptmalloc
echo ""
