#include <msgpack.hpp>

namespace type_msgpack {


struct Test {
	uint32_t id;
    std::string name;

    // nessary serialization defination
	MSGPACK_DEFINE(id, name);
};


}

