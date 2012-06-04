#include <pthread.h>
#include <unistd.h>
#include <sys/time.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include <string>

#define REQ_VEC 100000
size_t MAX_ALLOC_SIZE;
size_t ALLOC_REQ;
size_t TEST_ROUND;

std::string exec(const char* cmd)
{
    FILE* pipe = popen(cmd, "r");
    if (!pipe) return "ERROR";
    // it should be enough
    char buffer[1024];
    int nread = 0;
    while (!feof(pipe))
    {
        nread += fread(buffer+nread, 1024, 1, pipe);
    }
    //buffer[nread] = '\0';

    pclose(pipe);
    return std::string(buffer);
}


void* thread_func(void* arg)
{
    for (int j = 0; j < TEST_ROUND; ++j)
    {
        // generate a random array of malloc reqeusts
        size_t alloc_size[ALLOC_REQ];
        for (int i = 0; i < ALLOC_REQ; ++i)
        {
            alloc_size[i] = rand() % MAX_ALLOC_SIZE;
        }

        // malloc
        void*  alloc_ptr[ALLOC_REQ];
        for (int i = 0; i < ALLOC_REQ; ++i)
        {
            alloc_ptr[i] = malloc(alloc_size[i]);
        }

        // free
        for (int i = 0; i < ALLOC_REQ; ++i)
        {
            free(alloc_ptr[i]);
        }
    }
}

int main(int argc, char* argv[])
{
    if (argc != 5)
    {
        printf("\tusage : <thread_num> <max_alloc_size> <alloc_req_num> <round>\n");
        exit(-1);
    }
    //printf("Pid = %d\n", getpid());

    const size_t THREAD_NUM = atoi(argv[1]);
    MAX_ALLOC_SIZE = atoi(argv[2]);
    ALLOC_REQ = atoi(argv[3]);
    TEST_ROUND = atoi(argv[4]);

    // generate a random array of malloc request
    
    struct timeval tv_begin, tv_end;
    gettimeofday(&tv_begin, NULL);

    pthread_t pids[THREAD_NUM];
    for (int i = 0; i < THREAD_NUM; ++i)
    {
        pthread_create(&pids[i], NULL, thread_func, NULL);
    }

    for (int i = 0; i < THREAD_NUM; ++i)
    {
        pthread_join(pids[i], NULL);
    }

    gettimeofday(&tv_end, NULL);
    printf("Time : %lf sec.\n", (tv_end.tv_sec - tv_begin.tv_sec) + (tv_end.tv_usec - tv_begin.tv_usec)/1000000.0);

    // print memory usage
    char cmd[100];
    sprintf(cmd, "ps aux | grep %s | head -n 1 | awk '{print $5}'", argv[0]);
    std::string vsz = exec(cmd);
    printf("Virtual memory : %s\n", vsz.c_str());

    sprintf(cmd, "ps aux | grep %s | head -n 1 | awk '{print $6}'", argv[0]);
    std::string rsz = exec(cmd);
    printf("Resident memory : %s\n", rsz.c_str());
    //pause();
}
