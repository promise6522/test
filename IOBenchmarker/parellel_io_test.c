#include <unistd.h>
#include <pthread.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#ifndef BUFF_SIZE
# define BUFF_SIZE 4096 
#endif

struct task_info
{
    int thread_id;
    void* src_mem;
    int bytes;
};

void io_job_sync(int id, void* src, size_t bytes)
{
    char filename[20];
    sprintf(filename, "bin/test_%d.txt\0", id);

    int fd = open(filename, O_RDWR | O_CREAT, 0644);

    int writen = 0;
    while(writen < bytes)
    {
        int nwrite;
        if (bytes >= BUFF_SIZE)
            nwrite = BUFF_SIZE;
        else
            nwrite = bytes;
        
        assert(nwrite = write(fd, (void*)((long)src + writen), nwrite));
        fsync(fd);

        writen += nwrite;
    }

    fsync(fd);

    close(fd);
}

void* thread_func(void* arg)
{
    int thread_id = ((task_info*)arg)->thread_id;
    void* src_mem = ((task_info*)arg)->src_mem;
    int bytes     = ((task_info*)arg)->bytes;

    io_job_sync(thread_id, src_mem, bytes);
}

int main(int argc, char* argv[])
{
    if (argc != 3)
    {
       printf("usage : %s <file_num> <file_size_mb>\n", argv[0], argv[1], argv[2]); 
       return -1;
    }

    int file_num = atoi(argv[1]);
    int file_size = atoi(argv[2]) * 1024 * 1024;// arg in megabytes

    void* pSrc = malloc(file_size);
    assert(pSrc);

#ifdef MULTI_THREAD
    int thread_num = file_num;
    const int kMaxThread = 8;
    pthread_t threads[kMaxThread];
    task_info tasks[kMaxThread];

    for (int i = 0; i < thread_num; ++i)
    {
        tasks[i].thread_id = i;
        tasks[i].src_mem = pSrc;
        tasks[i].bytes = file_size;

        pthread_t tid;
        pthread_create(&tid, NULL, thread_func, &tasks[i]);
        threads[i] = tid;
    }

    for (int i = 0; i < thread_num; ++i)
    {
        pthread_join(threads[i], NULL);
    }
#else
    for (int i = 0; i < file_num; ++i)
    {
        io_job_sync(i, pSrc, file_size);
    }
#endif

}
