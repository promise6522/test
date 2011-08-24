#include <string.h>
#include <sys/time.h>
#include <iostream>
#include <stdexcept>
#include <string>
#include <limits>
#include <sstream>

#include <msgpack.hpp>
#include "msgpack_def.h"//msgpack defination

#include <google/protobuf/io/coded_stream.h>
#include <google/protobuf/io/zero_copy_stream_impl.h>
#include "protobuf_def.pb.h"//pb generated file
#include "protobuf_def.pb.cc"//pb generated file

struct cpp_type 
{
    uint32_t id;
    std::string name;
};

class simple_timer {
public:
	void reset() { gettimeofday(&m_timeval, NULL); }
	double show_stat(size_t bufsz)
	{
		struct timeval endtime;
		gettimeofday(&endtime, NULL);
		double sec = (endtime.tv_sec - m_timeval.tv_sec)
			+ (double)(endtime.tv_usec - m_timeval.tv_usec) / 1000 / 1000;
		std::cout << "\ttakes "<< sec << " sec" << std::endl;
		std::cout << "\tsize: "<< (double(bufsz)/1024) << " KB" << std::endl;
		return sec;
	}
private:
	timeval m_timeval;
};


void test_msgpack(const cpp_type& to_pack, unsigned int num)
{
	type_msgpack::Test target;
	target.id       = to_pack.id;
    target.name     = to_pack.name;

	simple_timer timer;

	std::cout << "---- MessagePack serialize single" << std::endl;
	timer.reset();

    msgpack::sbuffer sbuf;
    msgpack::pack(sbuf, target);
	timer.show_stat(sbuf.size());

	std::cout << "---- MessagePack deserialize single" << std::endl;
	timer.reset();
    {
        msgpack::zone z;
        size_t off = 0;
        type_msgpack::Test msg;
        msgpack::object obj = msgpack::unpack(sbuf.data(), sbuf.size(), z, &off);
        msg = obj.convert();
    }
	timer.show_stat(sbuf.size());


	std::cout << "---- MessagePack serialize multi" << std::endl;
    sbuf.clear();
	timer.reset();

    for(unsigned int i=0; i < num; ++i)
        msgpack::pack(sbuf, target);

	timer.show_stat(sbuf.size());

	std::cout << "---- MessagePack deserialize multi" << std::endl;
	timer.reset();
	{
		msgpack::zone z;
		size_t off = 0;
		type_msgpack::Test msg;
		for(unsigned int i=0; i < num; ++i) {
			msgpack::object obj = msgpack::unpack(sbuf.data(), sbuf.size(), z, &off);
			msg = obj.convert();
		}
	}
	timer.show_stat(sbuf.size());
}


void test_protobuf(const cpp_type& to_pack, unsigned int num)
{
	type_protobuf::Test target;
	target.set_id(to_pack.id);
	target.set_name(to_pack.name);

	simple_timer timer;

	std::cout << "-- Protocol Buffers serialize single" << std::endl; timer.reset();
    std::string data;
    timer.reset();
    { 
        target.SerializeToString(&data);
    }
	timer.show_stat(data.size());

	std::cout << "-- Protocol Buffers deserialize single" << std::endl;
	timer.reset();
    {
        target.ParseFromString(data);
    }
	timer.show_stat(data.size());


	std::cout << "-- Protocol Buffers serialize multi" << std::endl;
    std::string raw;
	timer.reset();
	{
		google::protobuf::io::StringOutputStream output(&raw);
		google::protobuf::io::CodedOutputStream encoder(&output);
		for(unsigned int i=0; i < num; ++i) {
			encoder.WriteVarint32(target.ByteSize());
			target.SerializeToCodedStream(&encoder);
        }
	}
	timer.show_stat(raw.size());


	std::cout << "-- Protocol Buffers deserialize multi" << std::endl;
	timer.reset();
	{
		type_protobuf::Test msg;
		google::protobuf::io::ArrayInputStream input(raw.data(), raw.size());
		google::protobuf::io::CodedInputStream decoder(&input);
		decoder.SetTotalBytesLimit(std::numeric_limits<int>::max(), std::numeric_limits<int>::max());
		for(unsigned int i=0; i < num; ++i) {
			uint32_t limit = 0;
			decoder.ReadVarint32(&limit);
			int old = decoder.PushLimit(limit);
		    msg.ParseFromCodedStream(&decoder);
			decoder.PopLimit(old);
		}
	}
	timer.show_stat(raw.size());

}


int main(int argc, char* argv[])
{
    // takes 2 param, 
    // first param indicates the string field size
    // second param indicates the num of serialization/deserialization
    int field_size, num;
	if(argc == 3) {
	    field_size = atoi(argv[1]);
        std::cout << "size of the string field : "<< field_size <<" byte" << std::endl;
	    num = atoi(argv[2]);
        std::cout << "number of serialize/deserialize : "<< num << std::endl;
    }
    else {
		std::cout << "usage: "<<argv[0]<<" <size> <num>" << std::endl;
		exit(1);
	}

    cpp_type to_pack;
    to_pack.id = 33;
    to_pack.name = std::string(field_size, 'x');

	test_msgpack(to_pack, num);      std::cout << std::endl;
	test_protobuf(to_pack, num);     std::cout << std::endl;
}

