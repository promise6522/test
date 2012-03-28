#include <pthread.h>
#include <boost/thread.hpp>

#define READER_NUM 3
#define WRITER_NUM 1
#define LOOP_COUNT 10000000

// the shared counter
int counter = 0;
// mutex for the shared counter
#ifdef USE_RW_LOCK
pthread_rwlock_t mtx;
#elif defined USE_BOOST_LOCK
boost::shared_mutex mtx;
#else
pthread_mutex_t mtx;
#endif

void init_lock();
void rd_lock();
void rd_unlock();
void wr_lock();
void wr_unlock();
void destroy_lock();

void* reader_func(void* arg);
void* writer_func(void* arg);

int main()
{
    init_lock();

    pthread_t pids[READER_NUM + WRITER_NUM];
    // readers
    int index = 0;
    int i;
    for (i = 0; i < READER_NUM; ++i)
    {
        pthread_t pid;
        pthread_create(&pid, NULL, reader_func, NULL);
        pids[index++] = pid;
    }

    // writers
    for (i = 0; i < WRITER_NUM; ++i)
    {
        pthread_t pid;
        pthread_create(&pid, NULL, writer_func, NULL);
        pids[index++] = pid;
    }

    // wait for all readers/writer to end
    for (i = 0; i < READER_NUM + WRITER_NUM; ++i)
    {
        pthread_join(pids[i], NULL);
    }

    destroy_lock();
}

void* reader_func(void* arg)
{
    int read;
    int i;
    for (i = 0; i < LOOP_COUNT; ++i)
    {
        rd_lock();
        read = counter;
        rd_unlock();
    }
}

void* writer_func(void* arg)
{
    int i;
    for (i = 0; i < LOOP_COUNT; ++i)
    {
        wr_lock();
        ++counter;
        wr_unlock();
    }
}

void init_lock()
{
#ifdef USE_RW_LOCK
    pthread_rwlock_init(&mtx, NULL);
#elif defined USE_BOOST_LOCK
    //nothing
#else
    pthread_mutex_init(&mtx, NULL);
#endif
}


void rd_lock()
{
#ifdef USE_RW_LOCK
    pthread_rwlock_rdlock(&mtx);
#elif defined USE_BOOST_LOCK
    mtx.lock_shared();
#else
    pthread_mutex_lock(&mtx);
#endif
}


void rd_unlock()
{
#ifdef USE_RW_LOCK
    pthread_rwlock_unlock(&mtx);
#elif defined USE_BOOST_LOCK
    mtx.unlock_shared();
#else
    pthread_mutex_unlock(&mtx);
#endif
}


void wr_lock()
{
#ifdef USE_RW_LOCK
    pthread_rwlock_wrlock(&mtx);
#elif defined USE_BOOST_LOCK
    mtx.lock();
#else
    pthread_mutex_lock(&mtx);
#endif
}


void wr_unlock()
{
#ifdef USE_RW_LOCK
    pthread_rwlock_unlock(&mtx);
#elif defined USE_BOOST_LOCK
    mtx.unlock();
#else
    pthread_mutex_unlock(&mtx);
#endif
}


void destroy_lock()
{
#ifdef USE_RW_LOCK
    pthread_rwlock_destroy(&mtx);
#elif defined USE_BOOST_LOCK
    //nothing
#else
    pthread_mutex_destroy(&mtx);
#endif
}
