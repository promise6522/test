#include <assert.h>
#include <errno.h>
#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include <vector>

const uint64_t kUpper = 0x1000000000;
const uint64_t kPthreadStackSize = 16384;

// advance = kUpper/thread_num, we only write it in the main thread,
// then readonly from spawned threads
uint64_t advance;

void* func(void* args);

void set_pthread_stack_size(pthread_attr_t *attr, size_t newsize)
{
    // get current stack size
    size_t stacksize;
    pthread_attr_init(attr);
    pthread_attr_getstacksize(attr, &stacksize);
    printf("Default stack size = %u\n", stacksize);

//#ifdef _POSIX_THREAD_ATTR_STACKSIZE
    // set new stack size
    if (0 != pthread_attr_setstacksize(attr, newsize))
    {
        printf("%s\n", strerror(errno));
        assert(false);
    } 
//#endif

    pthread_attr_getstacksize(attr, &stacksize);
    printf("New stack size = %u\n", stacksize);
}

int main(int argc, char* argv[])
{
    int thread_num;

    if (argc == 1)
    {
        // default one thread if no thread num is specified
        thread_num = 1;
    }
    else if (argc == 2)
    {
        thread_num = atoi(argv[1]);
    }
    else
        return -1;

    printf("Num of threads to spawn is %d\n", thread_num);

    // tune pthread stack size to support more threads
    pthread_attr_t attr;
    set_pthread_stack_size(&attr, kPthreadStackSize);

    // divide the kUpper by the thread num
    advance = kUpper/thread_num;

    std::vector<pthread_t> vThreads(thread_num);

    for (int i = 0; i < thread_num; ++i)
    {
        pthread_t tid;

        // the index for the thread
        int* pArg = (int*)malloc(sizeof(int));
        *pArg = i;

        if (0 != pthread_create(&tid, &attr, func, pArg))
        {
            printf("%s\n", strerror(errno));
            printf("Max num of created thread is %d\n", i);
            return -1;
        }

        vThreads[i] = tid;
    }

    uint64_t total = 0;
    for (int i = 0; i < thread_num; ++i)
    {
        void* part;
        assert(0 == pthread_join(vThreads[i], &part));
        total += *(uint64_t*)part;

        free((uint64_t*)part);
    }

    printf("The reuslt is %lu\n", total);

    return 0;
}

void* func(void* args)
{
    int index = *((int*)args);
    free((int*)args);

    uint64_t start = index * advance;

    uint64_t sum = 0;
    for (uint64_t i = 0; i < advance; ++i)
    {
        sum += start;
        start++;
    }

    uint64_t* pSum = (uint64_t*)malloc(sizeof(uint64_t));
    *pSum = sum;
    return pSum;
}
