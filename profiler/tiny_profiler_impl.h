#ifndef __TINY_PROFILER_IMPL
#define __TINY_PROFILER_IMPL

#include <stdlib.h>
#include <sys/time.h>

#include <vector>
#include <utility>

#include <boost/noncopyable.hpp>
#include <boost/thread.hpp>//for mutex & lock_guard


namespace tprof {

namespace detail {

/// A simple multi-producer, rare-consumer container:
/// Sacrifice the consumer's performance to improve producers'
template<typename ValueType, int N>
class RandomConcurrentVector : private boost::noncopyable {
public:
    void push_back(const ValueType& v) {
        int slot = rand() % N;
        boost::lock_guard<boost::mutex> lock(mtxs_[slot]);
        vecs_[slot].push_back(v);
    }

    void fetch_all(std::vector<ValueType>& cont) {
        // we don't preserve the insertion-order
        for (int slot = 0; slot < N; ++slot) {
            boost::lock_guard<boost::mutex> lock(mtxs_[slot]);
            cont.insert(cont.end(), vecs_[slot].begin(),
                        vecs_[slot].end());
        }
    }

private:
    boost::mutex mtxs_[N];
    std::vector<ValueType> vecs_[N];
};

class RecordCollector : private boost::noncopyable
{
public:
    static RecordCollector& instance() {
        static RecordCollector ins;
        return ins;
    }

    void collect(const char* name, long uelapse) {
        recVec_.push_back(std::make_pair(name, uelapse));
    }
private:
    RecordCollector() {
    }

private:
    typedef std::pair<const char*, long> ValueType;
    RandomConcurrentVector<ValueType, 5> recVec_;//5 is prime
};

class ProfRecord : private boost::noncopyable
{
public:
    ProfRecord(const char* name) : name_(name) {
        struct timeval tv;
        gettimeofday(&tv, NULL);
        ustart_ = tv.tv_sec*1000000 + tv.tv_usec;
    }

    ~ProfRecord() {
        struct timeval tv;
        gettimeofday(&tv, NULL);
        long uelapse = tv.tv_sec*10000000 + tv.tv_usec - ustart_;
        RecordCollector::instance().collect(name_, uelapse);
    }

private:
    const char* name_;
    long ustart_;
};



} /* detail */

}

#endif
