#include <pthread.h>
#include <stdlib.h>
#include <assert.h>
#include <iostream>

#include <queue>
#include <tbb/concurrent_queue.h>

int kThreadNum = 4;

int kPushNum = 1000000;


// the shared queue for push test
#ifdef USE_TBB
tbb::concurrent_queue<int> q;
#else
std::queue<int> q;
  #ifdef USE_SPINLOCK
pthread_spinlock_t q_mutex;
  #else
pthread_mutex_t q_mutex;
  #endif
#endif

void* pthread_func(void* arg)
{
    for (int i = 0; i < kPushNum; ++i)
    {
#ifdef USE_TBB
        q.push(0);
#else
  #ifdef USE_SPINLOCK
        pthread_spin_lock(&q_mutex);
        q.push(0);
        pthread_spin_unlock(&q_mutex);

  #else
        pthread_mutex_lock(&q_mutex);
        q.push(0);
        pthread_mutex_unlock(&q_mutex);
  #endif
#endif
    }

    return NULL;
}

int main(int argc, char* argv[])
{
    // thread num
    kThreadNum = atoi(argv[1]);
    // push num
    kPushNum = atoi(argv[2]);
    std::cout << "ThreadNum : " << kThreadNum << ", PushNum = " << kPushNum << std::endl;


#ifndef USE_TBB
  #ifdef USE_SPINLOCK
    pthread_spin_init(&q_mutex, PTHREAD_PROCESS_SHARED);
  #else
    // init mutex
    pthread_mutex_init(&q_mutex, NULL);
  #endif
#endif

    std::vector<pthread_t> tids;
    for(int i = 0; i < kThreadNum; ++i)
    {
        pthread_t tid;
        assert(0 == pthread_create(&tid, NULL, pthread_func, NULL));
        tids.push_back(tid);
    }

    // wait for all threads to end
    for(int i = 0; i < kThreadNum; ++i)
        pthread_join(tids[i], NULL);

}
