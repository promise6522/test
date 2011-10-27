#! /bin/bash

gcc fork.c task.c -o fork_test
gcc pthread.c task.c -o pthread_test -lpthread

echo "Executing fork_test..."
time ./fork_test

echo "Executing pthread_test..."
time ./pthread_test

rm fork_test
rm pthread_test
