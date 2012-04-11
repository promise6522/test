#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/time.h>

#define TOTAL_BUF_SIZE 200*1024*1024

int main(int argc, char* argv[])
{
    char* buf = (char*)malloc(TOTAL_BUF_SIZE * sizeof(char));
    assert(buf != 0);

    int set;
    if (argc == 1 || argv[1] == 0)
    {
        set = TOTAL_BUF_SIZE;
    }
    else
    {
        set = atoi(argv[1]);
    }

    struct timeval tv_begin, tv_end;
    gettimeofday(&tv_begin, NULL);

    int i = 0;
    for(; i < TOTAL_BUF_SIZE/set; ++i)
    {
        memset(buf/* + set*i*/, 0, set);
    }

    gettimeofday(&tv_end, NULL);

    printf("Used time : %ld us\n", (tv_end.tv_sec - tv_begin.tv_sec)*1000000 + (tv_end.tv_usec - tv_begin.tv_usec));

}
