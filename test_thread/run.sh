#! /bin/bash
#echo "Compiling..."
#g++ test.cpp -lpthread -g
g++ singleton.cpp -lpthread -o ./bin/singleton_safe
g++ singleton.cpp -lpthread -fno-threadsafe-statics -o ./bin/singleton_unsafe
