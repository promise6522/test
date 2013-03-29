#include <iostream>
#include <boost/bind.hpp>
#include <boost/thread.hpp>

#include "mordor/fibersynchronization.h"
#include "mordor/future.h"
#include "mordor/iomanager.h"
#include "mordor/thread.h"
#include "mordor/version.h"

using namespace Mordor;

#ifdef FIBER_SYNC
FiberMutex mtx;
#else
boost::mutex mtx;
#endif
long global = 0;

void func() {
    int i = 100;
    while (i-- > 0) {
        {
#ifdef FIBER_SYNC
            FiberMutex::ScopedLock lock(mtx);
#else
            boost::lock_guard<boost::mutex> lock(mtx);
#endif
            ++global;
        }

        Scheduler::yield();
    }
}

int main() {
    IOManager manager(2);
    for (int i = 0; i < 1000; ++i) {
        Fiber::ptr fib(new Fiber(func));
        manager.schedule(fib);
    }
    //manager.dispatch();
    manager.start();
    manager.stop();

    std::cout << "global = " << global << std::endl;
    return 0;
}
