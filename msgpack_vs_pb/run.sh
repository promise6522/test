#! /bin/bash

#specify your prefix of msgpack here
msgpack=/usr 
#specify your prefix of protobuf here
protobuf=/usr

#generate the protobuf headers and impls
echo "$0 : generating protobuf files..."
protobuf_gen_dir=./protobuf_gen
mkdir $protobuf_gen_dir -p #no error if existing
protoc protobuf_def.proto --cpp_out=$protobuf_gen_dir/

#compile
echo "$0 : compiling..."
g++ -Wall -O2 benchmark.cpp -o ./bin/benchmark -lpthread \
    -I$protobuf_gen_dir \
	-I$msgpack/include  -L$msgpack/lib  -lmsgpack -lmsgpackc \
	-I$protobuf/include -L$protobuf/lib -lprotobuf \

#run the object
echo "$0 : running the program..."
#field_size = 512B  test_num = 1000000
./bin/benchmark 512 1000000

