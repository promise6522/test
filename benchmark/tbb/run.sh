#! /usr/bin/env bash
g++ benchmark.cpp -lpthread -o mutex_queue -O2
g++ benchmark.cpp -lpthread -DUSE_TBB -ltbb -o tbb_queue -O2
g++ benchmark.cpp -lpthread -o spin_queue -O2

echo "For std::queue with mutex :"
time ./mutex_queue $1 $2

echo " "
echo "==========================="
echo " "

echo "For std::queue with spinlock :"
time ./spin_queue $1 $2

echo " "
echo "==========================="
echo " "

echo "For tbb::concurrent_queue :"
time ./tbb_queue $1 $2

