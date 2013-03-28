#include <sys/time.h>

#include <iostream>
#include <boost/bind.hpp>
#include <boost/thread.hpp>

#include "mordor/iomanager.h"
#include "mordor/thread.h"

using namespace Mordor;

void func(int switches) {
    int i = switches;
    while (i--) {
       Scheduler::yield();
    }
}

int main(int argc, char* argv[]) {
    int thread_num = atoi(argv[1]);
    int coro_num = atoi(argv[2]);
    int switch_per_coro = atoi(argv[3]);

    IOManager manager(thread_num);
    for (int i = 0; i < coro_num; ++i) {
        manager.schedule(boost::bind(func, switch_per_coro));
    }

    struct timeval tm_start, tm_end;
    gettimeofday(&tm_start, NULL);
    //
    manager.start();
    manager.stop();
    //
    gettimeofday(&tm_end, NULL);
    long us = (tm_end.tv_sec - tm_start.tv_sec)*1000000 +
        (tm_end.tv_usec - tm_start.tv_usec);

    std::cout << "####Benchmark result####" << std::endl;
    std::cout << "Threads : " << thread_num << std::endl;
    std::cout << "Coroutines : " << coro_num << std::endl;
    std::cout << "Totol switches : " << coro_num * switch_per_coro << std::endl;
    std::cout << "Cost per switch(us) : " << (double)us/(coro_num*switch_per_coro) << std::endl;
    std::cout << "####Benchmark result####" << std::endl;
    return 0;
}
