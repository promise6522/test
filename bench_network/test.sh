#! /bin/bash

if [ $# -ne 1 ]
then
    echo "Enter the packet size:"
    exit -1
fi
# run receiver in background
./server $1 &

# luanch the sender
ssh tom@192.168.1.19 /home/tom/test/bench_network/client 192.168.1.18
