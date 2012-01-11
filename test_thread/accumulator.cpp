#include <assert.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <pthread.h>

#ifndef CACHE_LINE_SIZE
# define CACHE_LINE_SIZE 64
#endif

// A template for data-object
// when created on stack or in data seg.
// guarantees object is aligned on cache-line (and fills it)
template<typename T>
struct CacheLineStorage
{
    // if posible, using C++0x alignment syntax
    // unfortunately, alignment is not supported by GCC yet
#ifdef __GXX_EXPERIMENTAL_CXX0X__
    [[ align(CACHE_LINE_SIZE) ]] T data;
#else
    __attribute__(( aligned(CACHE_LINE_SIZE) )) T data;
#endif

    // pad bytes to fill the rest of the cache line
    char __pad[ CACHE_LINE_SIZE > sizeof(T) ? CACHE_LINE_SIZE - sizeof(T) : 1 ];

    // Test if acturally aligned
    // CacheLineStorage<int> cls_int;
    // assert(reinterpret_cast<uint64_t>(&cls_int) % CACHE_LINE_SIZE == 0);
    // assert(sizeof(cls_int) >= CACHE_LINE_SIZE );
};

struct task
{
    CacheLineStorage<uint64_t>* pSum;
    uint64_t  start;
    uint64_t  end;
    int       id;
};

void* accumulator_func(void* arg)
{
    // get all the params and free the mem
    task* pTask = (task*)arg;
    CacheLineStorage<uint64_t>* pSum = pTask->pSum;
    uint64_t start = pTask->start;
    uint64_t end   = pTask->end;
    int id         = pTask->id;
    delete pTask;

    // do the computation work
    for(int i = start; i <= end; ++i)
            pSum->data += i;

    printf("Thread %d has finished.\n", id);
}

const int MAX_PARELLEL = 10000;

int main(int argc, char* argv[])
{
    if (argc != 3)
    {
        printf("usage : ./accumulator <num of threads> <maximum>\n");
        return -1;
    }
    int parellel = atoi(argv[1]);
    int max = atoi(argv[2]);

    //uint64_t results[MAX_PARELLEL];
    CacheLineStorage<uint64_t> results[MAX_PARELLEL];
    pthread_t tids[MAX_PARELLEL];
    for (int i = 0; i < parellel; ++i)
    {
        results[i].data = 0;
        // build the arg
        task* pTask = new task();
        pTask->pSum = &results[i];
        pTask->start = max/parellel * i + 1;
        pTask->end = max/parellel * (i + 1);
        pTask->id = i;

        pthread_t tid;
        pthread_create(&tid, NULL, accumulator_func, pTask);
        tids[i] = tid;
    }

    // wait for all the threads to finish, and collect the results
    uint64_t sum = 0;
    for (int i = 0; i < parellel; ++i)
    {
        pthread_join(tids[i], NULL);
        sum += results[i].data;
    }

    printf("The sum from 1 to %d is %ld\n", max, sum);
}
